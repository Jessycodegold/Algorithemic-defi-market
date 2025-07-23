;; Algorithemic-market-contract
;; A dynamic market maker that adjusts parameters based on market conditions

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-pool-exists (err u102))
(define-constant err-pool-not-found (err u103))
(define-constant err-token-exists (err u104))
(define-constant err-token-not-found (err u105))
(define-constant err-insufficient-balance (err u106))
(define-constant err-zero-amount (err u107))
(define-constant err-price-impact-too-high (err u108))
(define-constant err-slippage-too-high (err u109))
(define-constant err-insufficient-liquidity (err u110))
(define-constant err-invalid-parameters (err u111))
(define-constant err-range-invalid (err u112))
(define-constant err-position-not-found (err u113))
(define-constant err-emergency-shutdown (err u114))
(define-constant err-min-deposit (err u115))
(define-constant err-position-still-active (err u116))
(define-constant err-paused (err u117))
(define-constant err-oracle-error (err u118))
(define-constant err-oracle-stale (err u119))
(define-constant err-unauthorized-oracle (err u120))
(define-constant err-invalid-curve (err u121))
(define-constant err-range-outside-pool (err u122))
(define-constant err-rewards-claimed-already (err u123))
(define-constant err-no-rewards-available (err u124))

;; Protocol parameters
(define-data-var next-pool-id uint u1)
(define-data-var next-position-id uint u1)
(define-data-var protocol-fee-bp uint u30) ;; 0.3% base protocol fee in basis points
(define-data-var min-deposit-amount uint u1000000) ;; 1 STX minimum deposit
(define-data-var emergency-shutdown bool false)
(define-data-var treasury-address principal contract-owner)
(define-data-var volatility-update-frequency uint u144) ;; Update volatility approx once per day
(define-data-var max-price-impact-bp uint u300) ;; 3% max price impact
(define-data-var impermanent-loss-threshold uint u500) ;; 5% threshold for IL protection
(define-data-var impermanent-loss-coverage-bp uint u5000) ;; 50% IL coverage
(define-data-var dynamic-range-adjustment-factor uint u500) ;; 5% range adjustment
(define-data-var price-deviation-threshold uint u200) ;; 2% threshold for price deviation
(define-data-var max-dynamic-fee-increase uint u500) ;; Maximum 5% fee increase

;; Curve types enumeration
;; 0 = Constant Product (x*y=k), 1 = Stableswap, 2 = Exponential, 3 = Dynamic
(define-data-var curve-types (list 4 (string-ascii 20)) (list "ConstantProduct" "Stableswap" "Exponential" "Dynamic"))

;; Pool status enumeration
;; 0 = Active, 1 = Paused, 2 = Deprecated
(define-data-var pool-statuses (list 3 (string-ascii 10)) (list "Active" "Paused" "Deprecated"))

;; Range status enumeration
;; 0 = Out-of-range, 1 = In-range, 2 = Partial-range
(define-data-var range-statuses (list 3 (string-ascii 15)) (list "Out-of-range" "In-range" "Partial-range"))

;; Supported tokens
(define-map token-registry
  { token-id: (string-ascii 20) }
  {
    name: (string-ascii 40),
    token-type: (string-ascii 10), ;; "fungible" or "non-fungible"
    contract: principal,
    decimals: uint,
    price-oracle: principal,
    volatility-history: (list 30 uint), ;; Recent volatility measurements
    current-volatility: uint, ;; Current volatility score 1-10000
    is-stable: bool, ;; Is this a stablecoin
    max-supply: uint,
    last-price: uint, ;; Last price in STX with 8 decimals
    last-update-block: uint
  }
)
;; Liquidity pools
(define-map liquidity-pools
  { pool-id: uint }
  {
    token-x: (string-ascii 20),
    token-y: (string-ascii 20),
    reserve-x: uint,
    reserve-y: uint,
    virtual-reserve-x: uint, ;; Used for stableswap and custom curves
    virtual-reserve-y: uint,
    liquidity-units: uint, ;; Total liquidity shares
    curve-type: uint,
    curve-params: (list 5 uint), ;; Custom parameters for the curve
    base-fee-bp: uint, ;; Base fee in basis points
    dynamic-fee-bp: uint, ;; Additional dynamic fee based on volatility
    current-tick: int, ;; Current price tick (log base 1.0001 of price)
    tick-spacing: uint, ;; Minimum tick movement
    price-oracle: principal,
    total-volume-x: uint,
    total-volume-y: uint,
    total-fees-x: uint,
    total-fees-y: uint,
    total-fees-protocol: uint,
    creation-block: uint,
    last-update-block: uint,
    status: uint, ;; 0=Active, 1=Paused, 2=Deprecated
    price-history: (list 24 { price: uint, timestamp: uint }), ;; 24 hours of price history
    volatility-adjustment: uint, ;; Dynamic adjustment to fee and ranges
    concentrated-ranges: (list 10 { 
      tick-lower: int, 
      tick-upper: int, 
      liquidity: uint, 
      positions-count: uint 
    }),
    total-il-compensation-paid: uint
  }
)

;; Liquidity positions
(define-map liquidity-positions
  { position-id: uint }
  {
    pool-id: uint,
    provider: principal,
    liquidity-units: uint, ;; Share of the pool
    token-x-amount: uint,
    token-y-amount: uint,
    entry-price: uint, ;; Entry price for impermanent loss calculation
    entry-sqrt-price: uint, ;; Square root of price at entry (for concentrated liquidity)
    entry-block: uint,
    last-update-block: uint,
    tick-lower: int, ;; Lower tick bound for concentrated liquidity
    tick-upper: int, ;; Upper tick bound for concentrated liquidity
    range-status: uint, ;; 0=Out-of-range, 1=In-range, 2=Partial-range
    fees-earned-x: uint,
    fees-earned-y: uint,
    rewards-earned: uint,
    rewards-claimed: uint,
    il-compensation: uint,
    is-concentrated: bool ;; Is this a concentrated liquidity position
  }
)

;; User positions index
(define-map user-positions
  { user: principal }
  { position-ids: (list 100 uint) }
)

;; Pool positions index
(define-map pool-positions
  { pool-id: uint }
  { position-ids: (list 500 uint) }
)

;; Oracle price data
(define-map oracle-prices
  { token-id: (string-ascii 20) }
  {
    price: uint, ;; Price in STX with 8 decimals
    last-update-block: uint,
    twap-price: uint, ;; Time-weighted average price
    trusted: bool,
    oracle-address: principal
  }
)