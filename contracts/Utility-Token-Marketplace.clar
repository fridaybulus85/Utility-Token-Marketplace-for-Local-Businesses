(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-business-not-found (err u106))
(define-constant err-token-not-found (err u107))
(define-constant err-insufficient-tokens (err u108))
(define-constant err-invalid-price (err u109))
(define-constant err-order-not-found (err u110))
(define-constant err-stake-not-found (err u111))
(define-constant err-stake-still-locked (err u112))
(define-constant err-invalid-duration (err u113))

(define-constant reward-tier-1-threshold u5)
(define-constant reward-tier-2-threshold u15)
(define-constant reward-tier-3-threshold u30)
(define-constant loyalty-bonus-blocks u144)
(define-constant err-no-rewards (err u200))

(define-constant err-flash-loan-not-repaid (err u300))
(define-constant err-flash-loan-insufficient-fee (err u301))
(define-constant err-flash-loan-active (err u302))
(define-constant err-flash-loan-invalid-callback (err u303))

(define-constant err-buyback-not-found (err u400))
(define-constant err-buyback-inactive (err u401))
(define-constant err-insufficient-buyback-balance (err u402))
(define-constant err-below-minimum-buyback (err u403))

(define-constant err-escrow-not-found (err u500))
(define-constant err-escrow-already-settled (err u501))
(define-constant err-escrow-not-expired (err u502))
(define-constant err-invalid-party (err u503))

(define-map businesses
  { business-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    active: bool,
    created-at: uint
  }
)

(define-map utility-tokens
  { token-id: uint }
  {
    business-id: uint,
    name: (string-ascii 50),
    symbol: (string-ascii 10),
    description: (string-ascii 200),
    total-supply: uint,
    price-per-token: uint,
    active: bool,
    created-at: uint
  }
)

(define-map token-balances
  { token-id: uint, owner: principal }
  { balance: uint }
)

(define-map trade-orders
  { order-id: uint }
  {
    seller: principal,
    token-id: uint,
    amount: uint,
    price-per-token: uint,
    active: bool,
    created-at: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    name: (string-ascii 50),
    total-trades: uint,
    reputation-score: uint,
    created-at: uint
  }
)

(define-data-var next-business-id uint u1)
(define-data-var next-token-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-public (register-business (name (string-ascii 50)) (description (string-ascii 200)))
  (let
    (
      (business-id (var-get next-business-id))
      (current-height stacks-block-height)
    )
    (asserts! (> (len name) u0) err-invalid-amount)
    (map-set businesses
      { business-id: business-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        active: true,
        created-at: current-height
      }
    )
    (var-set next-business-id (+ business-id u1))
    (ok business-id)
  )
)

(define-public (create-utility-token 
  (business-id uint) 
  (name (string-ascii 50)) 
  (symbol (string-ascii 10)) 
  (description (string-ascii 200)) 
  (total-supply uint) 
  (price-per-token uint))
  (let
    (
      (token-id (var-get next-token-id))
      (business (unwrap! (map-get? businesses { business-id: business-id }) err-business-not-found))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner business)) err-unauthorized)
    (asserts! (get active business) err-business-not-found)
    (asserts! (> total-supply u0) err-invalid-amount)
    (asserts! (> price-per-token u0) err-invalid-price)
    (map-set utility-tokens
      { token-id: token-id }
      {
        business-id: business-id,
        name: name,
        symbol: symbol,
        description: description,
        total-supply: total-supply,
        price-per-token: price-per-token,
        active: true,
        created-at: current-height
      }
    )
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: total-supply }
    )
    (var-set next-token-id (+ token-id u1))
    (ok token-id)
  )
)

(define-public (purchase-tokens (token-id uint) (amount uint))
  (let
    (
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id token) }) err-business-not-found))
      (business-owner (get owner business))
      (total-cost (* amount (get price-per-token token)))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (business-payment (- total-cost platform-fee))
      (current-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: tx-sender }))))
      (business-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: business-owner }))))
    )
    (asserts! (get active token) err-token-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= business-balance amount) err-insufficient-tokens)
    (try! (stx-transfer? total-cost tx-sender contract-owner))
    (try! (stx-transfer? business-payment contract-owner business-owner))
    (map-set token-balances
      { token-id: token-id, owner: business-owner }
      { balance: (- business-balance amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-public (create-trade-order (token-id uint) (amount uint) (price-per-token uint))
  (let
    (
      (order-id (var-get next-order-id))
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (seller-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: tx-sender }))))
      (current-height stacks-block-height)
    )
    (asserts! (get active token) err-token-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-token u0) err-invalid-price)
    (asserts! (>= seller-balance amount) err-insufficient-tokens)
    (map-set trade-orders
      { order-id: order-id }
      {
        seller: tx-sender,
        token-id: token-id,
        amount: amount,
        price-per-token: price-per-token,
        active: true,
        created-at: current-height
      }
    )
    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

(define-public (execute-trade-order (order-id uint))
  (let
    (
      (order (unwrap! (map-get? trade-orders { order-id: order-id }) err-order-not-found))
      (seller (get seller order))
      (token-id (get token-id order))
      (amount (get amount order))
      (price-per-token (get price-per-token order))
      (total-cost (* amount price-per-token))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (seller-payment (- total-cost platform-fee))
      (seller-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: seller }))))
      (buyer-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: tx-sender }))))
    )
    (asserts! (get active order) err-order-not-found)
    (asserts! (not (is-eq tx-sender seller)) err-unauthorized)
    (asserts! (>= seller-balance amount) err-insufficient-tokens)
    (try! (stx-transfer? total-cost tx-sender contract-owner))
    (try! (stx-transfer? seller-payment contract-owner seller))
    (map-set token-balances
      { token-id: token-id, owner: seller }
      { balance: (- seller-balance amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (+ buyer-balance amount) }
    )
    (map-set trade-orders
      { order-id: order-id }
      (merge order { active: false })
    )
    (update-user-reputation seller)
    (update-user-reputation tx-sender)
    (ok true)
  )
)

(define-public (redeem-tokens (token-id uint) (amount uint))
  (let
    (
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id token) }) err-business-not-found))
      (user-balance (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: tx-sender }))))
    )
    (asserts! (get active token) err-token-not-found)
    (asserts! (get active business) err-business-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= user-balance amount) err-insufficient-tokens)
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (- user-balance amount) }
    )
    (ok true)
  )
)

(define-public (cancel-trade-order (order-id uint))
  (let
    (
      (order (unwrap! (map-get? trade-orders { order-id: order-id }) err-order-not-found))
    )
    (asserts! (is-eq tx-sender (get seller order)) err-unauthorized)
    (asserts! (get active order) err-order-not-found)
    (map-set trade-orders
      { order-id: order-id }
      (merge order { active: false })
    )
    (ok true)
  )
)

(define-public (create-user-profile (name (string-ascii 50)))
  (let
    (
      (current-height stacks-block-height)
    )
    (asserts! (> (len name) u0) err-invalid-amount)
    (asserts! (is-none (map-get? user-profiles { user: tx-sender })) err-already-exists)
    (map-set user-profiles
      { user: tx-sender }
      {
        name: name,
        total-trades: u0,
        reputation-score: u100,
        created-at: current-height
      }
    )
    (ok true)
  )
)

(define-private (update-user-reputation (user principal))
  (let
    (
      (profile (default-to 
        { name: "", total-trades: u0, reputation-score: u100, created-at: stacks-block-height }
        (map-get? user-profiles { user: user })
      ))
      (new-trades (+ (get total-trades profile) u1))
      (reputation-bonus (if (> new-trades u10) u5 u1))
      (new-reputation (+ (get reputation-score profile) reputation-bonus))
    )
    (map-set user-profiles
      { user: user }
      (merge profile { 
        total-trades: new-trades,
        reputation-score: (if (> new-reputation u1000) u1000 new-reputation)
      })
    )
    true
  )
)

(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount)
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-business (business-id uint))
  (map-get? businesses { business-id: business-id })
)

(define-read-only (get-utility-token (token-id uint))
  (map-get? utility-tokens { token-id: token-id })
)

(define-read-only (get-token-balance (token-id uint) (owner principal))
  (default-to u0 (get balance (map-get? token-balances { token-id: token-id, owner: owner })))
)

(define-read-only (get-trade-order (order-id uint))
  (map-get? trade-orders { order-id: order-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-business-id)
  (var-get next-business-id)
)

(define-read-only (get-next-token-id)
  (var-get next-token-id)
)

(define-read-only (get-next-order-id)
  (var-get next-order-id)
)


(define-map token-stakes
  { stake-id: uint }
  {
    staker: principal,
    token-id: uint,
    amount: uint,
    stake-duration: uint,
    start-block: uint,
    end-block: uint,
    active: bool
  }
)

(define-data-var next-stake-id uint u1)

(define-public (stake-tokens (token-id uint) (amount uint) (duration-blocks uint))
  (let
    (
      (stake-id (var-get next-stake-id))
      (current-block stacks-block-height)
      (end-block (+ current-block duration-blocks))
      (user-balance (get-token-balance token-id tx-sender))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= duration-blocks u144) err-invalid-duration)
    (asserts! (>= user-balance amount) err-insufficient-tokens)
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (- user-balance amount) }
    )
    (map-set token-stakes
      { stake-id: stake-id }
      {
        staker: tx-sender,
        token-id: token-id,
        amount: amount,
        stake-duration: duration-blocks,
        start-block: current-block,
        end-block: end-block,
        active: true
      }
    )
    (var-set next-stake-id (+ stake-id u1))
    (ok stake-id)
  )
)

(define-public (unstake-tokens (stake-id uint))
  (let
    (
      (stake (unwrap! (map-get? token-stakes { stake-id: stake-id }) err-stake-not-found))
      (current-block stacks-block-height)
      (staker (get staker stake))
      (token-id (get token-id stake))
      (amount (get amount stake))
      (duration (get stake-duration stake))
      (reward-multiplier (if (>= duration u1008) u120 (if (>= duration u720) u110 u105)))
      (reward-amount (/ (* amount (- reward-multiplier u100)) u100))
      (total-return (+ amount reward-amount))
      (current-balance (get-token-balance token-id staker))
    )
    (asserts! (is-eq tx-sender staker) err-unauthorized)
    (asserts! (get active stake) err-stake-not-found)
    (asserts! (>= current-block (get end-block stake)) err-stake-still-locked)
    (map-set token-balances
      { token-id: token-id, owner: staker }
      { balance: (+ current-balance total-return) }
    )
    (map-set token-stakes
      { stake-id: stake-id }
      (merge stake { active: false })
    )
    (ok total-return)
  )
)

(define-read-only (get-stake (stake-id uint))
  (map-get? token-stakes { stake-id: stake-id })
)

(define-read-only (calculate-stake-reward (stake-id uint))
  (match (map-get? token-stakes { stake-id: stake-id })
    stake (let
      (
        (duration (get stake-duration stake))
        (amount (get amount stake))
        (multiplier (if (>= duration u1008) u120 (if (>= duration u720) u110 u105)))
      )
      (ok (/ (* amount (- multiplier u100)) u100))
    )
    err-stake-not-found
  )
)

(define-map user-rewards
  { user: principal, token-id: uint }
  {
    total-purchased: uint,
    purchase-count: uint,
    last-purchase-block: uint,
    accumulated-bonus: uint,
    tier-level: uint
  }
)

(define-private (calculate-reward-tier (purchase-count uint) (total-purchased uint))
  (if (>= purchase-count reward-tier-3-threshold)
    u3
    (if (>= purchase-count reward-tier-2-threshold)
      u2
      (if (>= purchase-count reward-tier-1-threshold)
        u1
        u0
      )
    )
  )
)

(define-private (calculate-bonus-tokens (amount uint) (tier uint) (is-loyal bool))
  (let
    (
      (base-bonus (/ (* amount tier) u100))
      (loyalty-multiplier (if is-loyal u2 u1))
    )
    (* base-bonus loyalty-multiplier)
  )
)

(define-private (update-user-rewards (user principal) (token-id uint) (amount uint))
  (let
    (
      (current-block stacks-block-height)
      (existing-rewards (default-to 
        { total-purchased: u0, purchase-count: u0, last-purchase-block: u0, accumulated-bonus: u0, tier-level: u0 }
        (map-get? user-rewards { user: user, token-id: token-id })
      ))
      (new-total (+ (get total-purchased existing-rewards) amount))
      (new-count (+ (get purchase-count existing-rewards) u1))
      (is-loyal (>= current-block (+ (get last-purchase-block existing-rewards) loyalty-bonus-blocks)))
      (new-tier (calculate-reward-tier new-count new-total))
      (bonus-tokens (calculate-bonus-tokens amount new-tier is-loyal))
      (new-bonus (+ (get accumulated-bonus existing-rewards) bonus-tokens))
    )
    (map-set user-rewards
      { user: user, token-id: token-id }
      {
        total-purchased: new-total,
        purchase-count: new-count,
        last-purchase-block: current-block,
        accumulated-bonus: new-bonus,
        tier-level: new-tier
      }
    )
    bonus-tokens
  )
)

(define-public (claim-reward-tokens (token-id uint))
  (let
    (
      (rewards (unwrap! (map-get? user-rewards { user: tx-sender, token-id: token-id }) err-no-rewards))
      (bonus-amount (get accumulated-bonus rewards))
      (current-balance (get-token-balance token-id tx-sender))
    )
    (asserts! (> bonus-amount u0) err-no-rewards)
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (+ current-balance bonus-amount) }
    )
    (map-set user-rewards
      { user: tx-sender, token-id: token-id }
      (merge rewards { accumulated-bonus: u0 })
    )
    (ok bonus-amount)
  )
)

(define-read-only (get-user-rewards (user principal) (token-id uint))
  (map-get? user-rewards { user: user, token-id: token-id })
)




(define-map flash-loans
  { loan-id: uint }
  {
    borrower: principal,
    token-id: uint,
    amount: uint,
    fee: uint,
    active: bool,
    created-block: uint
  }
)

(define-data-var next-loan-id uint u1)
(define-data-var flash-loan-fee-rate uint u50)

(define-public (execute-flash-loan (token-id uint) (amount uint) (callback-contract principal))
  (let
    (
      (loan-id (var-get next-loan-id))
      (current-block stacks-block-height)
      (fee (/ (* amount (var-get flash-loan-fee-rate)) u10000))
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id token) }) err-business-not-found))
      (business-owner (get owner business))
      (available-balance (get-token-balance token-id business-owner))
    )
    (asserts! (get active token) err-token-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= available-balance amount) err-insufficient-tokens)
    
    (map-set flash-loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        token-id: token-id,
        amount: amount,
        fee: fee,
        active: true,
        created-block: current-block
      }
    )
    (var-set next-loan-id (+ loan-id u1))
    
    (map-set token-balances
      { token-id: token-id, owner: business-owner }
      { balance: (- available-balance amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (+ (get-token-balance token-id tx-sender) amount) }
    )
    
    (try! (validate-flash-loan-repayment loan-id))
    (ok loan-id)
  )
)

(define-private (validate-flash-loan-repayment (loan-id uint))
  (let
    (
      (loan (unwrap! (map-get? flash-loans { loan-id: loan-id }) err-flash-loan-not-repaid))
      (borrower (get borrower loan))
      (token-id (get token-id loan))
      (amount (get amount loan))
      (fee (get fee loan))
      (required-balance (+ amount fee))
      (borrower-balance (get-token-balance token-id borrower))
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id token) }) err-business-not-found))
      (business-owner (get owner business))
      (business-balance (get-token-balance token-id business-owner))
    )
    (asserts! (get active loan) err-flash-loan-not-repaid)
    (asserts! (>= borrower-balance required-balance) err-flash-loan-not-repaid)
    
    (map-set token-balances
      { token-id: token-id, owner: borrower }
      { balance: (- borrower-balance required-balance) }
    )
    (map-set token-balances
      { token-id: token-id, owner: business-owner }
      { balance: (+ business-balance amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: contract-owner }
      { balance: (+ (get-token-balance token-id contract-owner) fee) }
    )
    
    (map-set flash-loans
      { loan-id: loan-id }
      (merge loan { active: false })
    )
    (ok true)
  )
)

(define-public (set-flash-loan-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount)
    (var-set flash-loan-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-flash-loan (loan-id uint))
  (map-get? flash-loans { loan-id: loan-id })
)

(define-read-only (calculate-flash-loan-fee (amount uint))
  (/ (* amount (var-get flash-loan-fee-rate)) u10000)
)

(define-read-only (get-flash-loan-fee-rate)
  (var-get flash-loan-fee-rate)
)

(define-map buyback-programs
  { program-id: uint }
  {
    business-id: uint,
    token-id: uint,
    buyback-price: uint,
    max-buyback-amount: uint,
    current-bought-back: uint,
    active: bool,
    created-at: uint
  }
)

(define-data-var next-buyback-id uint u1)

(define-public (create-buyback-program (token-id uint) (buyback-price uint) (max-buyback-amount uint))
  (let
    (
      (program-id (var-get next-buyback-id))
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id token) }) err-business-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get owner business)) err-unauthorized)
    (asserts! (get active token) err-token-not-found)
    (asserts! (> buyback-price u0) err-invalid-price)
    (asserts! (> max-buyback-amount u0) err-invalid-amount)
    (map-set buyback-programs
      { program-id: program-id }
      {
        business-id: (get business-id token),
        token-id: token-id,
        buyback-price: buyback-price,
        max-buyback-amount: max-buyback-amount,
        current-bought-back: u0,
        active: true,
        created-at: current-block
      }
    )
    (var-set next-buyback-id (+ program-id u1))
    (ok program-id)
  )
)

(define-public (execute-buyback (program-id uint) (amount uint))
  (let
    (
      (program (unwrap! (map-get? buyback-programs { program-id: program-id }) err-buyback-not-found))
      (token-id (get token-id program))
      (buyback-price (get buyback-price program))
      (remaining-capacity (- (get max-buyback-amount program) (get current-bought-back program)))
      (token (unwrap! (map-get? utility-tokens { token-id: token-id }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id program) }) err-business-not-found))
      (business-owner (get owner business))
      (user-balance (get-token-balance token-id tx-sender))
      (business-balance (get-token-balance token-id business-owner))
      (total-payout (* amount buyback-price))
    )
    (asserts! (get active program) err-buyback-inactive)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (<= amount remaining-capacity) err-insufficient-buyback-balance)
    (asserts! (>= user-balance amount) err-insufficient-tokens)
    (try! (stx-transfer? total-payout business-owner tx-sender))
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (- user-balance amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: business-owner }
      { balance: (+ business-balance amount) }
    )
    (map-set buyback-programs
      { program-id: program-id }
      (merge program { current-bought-back: (+ (get current-bought-back program) amount) })
    )
    (ok total-payout)
  )
)

(define-public (deactivate-buyback-program (program-id uint))
  (let
    (
      (program (unwrap! (map-get? buyback-programs { program-id: program-id }) err-buyback-not-found))
      (token (unwrap! (map-get? utility-tokens { token-id: (get token-id program) }) err-token-not-found))
      (business (unwrap! (map-get? businesses { business-id: (get business-id program) }) err-business-not-found))
    )
    (asserts! (is-eq tx-sender (get owner business)) err-unauthorized)
    (map-set buyback-programs
      { program-id: program-id }
      (merge program { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-buyback-program (program-id uint))
  (map-get? buyback-programs { program-id: program-id })
)

(define-read-only (get-next-buyback-id)
  (var-get next-buyback-id)
)

(define-map escrow-agreements
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    token-id: uint,
    token-amount: uint,
    stx-amount: uint,
    expiry-block: uint,
    settled: bool,
    created-at: uint
  }
)

(define-data-var next-escrow-id uint u1)

(define-public (create-escrow (seller principal) (token-id uint) (token-amount uint) (stx-amount uint) (duration-blocks uint))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-block stacks-block-height)
      (expiry-block (+ current-block duration-blocks))
      (buyer-token-balance (get-token-balance token-id tx-sender))
    )
    (asserts! (> token-amount u0) err-invalid-amount)
    (asserts! (> stx-amount u0) err-invalid-amount)
    (asserts! (> duration-blocks u0) err-invalid-duration)
    (asserts! (>= buyer-token-balance token-amount) err-insufficient-tokens)
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    (map-set token-balances
      { token-id: token-id, owner: tx-sender }
      { balance: (- buyer-token-balance token-amount) }
    )
    (map-set escrow-agreements
      { escrow-id: escrow-id }
      {
        buyer: tx-sender,
        seller: seller,
        token-id: token-id,
        token-amount: token-amount,
        stx-amount: stx-amount,
        expiry-block: expiry-block,
        settled: false,
        created-at: current-block
      }
    )
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (release-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) err-escrow-not-found))
      (buyer (get buyer escrow))
      (seller (get seller escrow))
      (token-id (get token-id escrow))
      (token-amount (get token-amount escrow))
      (stx-amount (get stx-amount escrow))
      (seller-balance (get-token-balance token-id seller))
    )
    (asserts! (is-eq tx-sender buyer) err-invalid-party)
    (asserts! (not (get settled escrow)) err-escrow-already-settled)
    (try! (as-contract (stx-transfer? stx-amount tx-sender seller)))
    (map-set token-balances
      { token-id: token-id, owner: seller }
      { balance: (- seller-balance token-amount) }
    )
    (map-set token-balances
      { token-id: token-id, owner: buyer }
      { balance: (+ (get-token-balance token-id buyer) token-amount) }
    )
    (map-set escrow-agreements
      { escrow-id: escrow-id }
      (merge escrow { settled: true })
    )
    (ok true)
  )
)

(define-public (cancel-escrow (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) err-escrow-not-found))
      (current-block stacks-block-height)
      (buyer (get buyer escrow))
      (token-id (get token-id escrow))
      (token-amount (get token-amount escrow))
      (stx-amount (get stx-amount escrow))
      (buyer-balance (get-token-balance token-id buyer))
    )
    (asserts! (or (is-eq tx-sender buyer) (is-eq tx-sender (get seller escrow))) err-invalid-party)
    (asserts! (not (get settled escrow)) err-escrow-already-settled)
    (asserts! (>= current-block (get expiry-block escrow)) err-escrow-not-expired)
    (try! (as-contract (stx-transfer? stx-amount tx-sender buyer)))
    (map-set token-balances
      { token-id: token-id, owner: buyer }
      { balance: (+ buyer-balance token-amount) }
    )
    (map-set escrow-agreements
      { escrow-id: escrow-id }
      (merge escrow { settled: true })
    )
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-agreements { escrow-id: escrow-id })
)

(define-read-only (get-next-escrow-id)
  (var-get next-escrow-id)
)