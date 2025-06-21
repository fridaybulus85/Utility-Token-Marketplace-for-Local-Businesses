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
