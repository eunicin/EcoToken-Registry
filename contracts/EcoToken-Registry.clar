;; ******************************************************************
;; EcoToken: Verified Carbon Offset Registry
;; ******************************************************************
;; This registry implements environmental asset tokenization using Clarity.
;; It enables creation, trading, retirement, and verification of 
;; eco-credits as non-fungible tokens with provenance tracking.
;;
;; Core Capabilities:
;; - Issuance: Authorized issuance of individual or grouped environmental assets
;; - Provenance: Links each asset to verifiable data through URI references
;; - Custody: Secure tracking of asset custody throughout lifecycle
;; - Retirement: Permanent retirement mechanism for claimed offsets
;; - Verification: Ensures data integrity with format validation and batch controls
;;
;; Security Measures:
;; - Admin-gated issuance to maintain market integrity
;; - Retirement safeguards preventing reuse of claimed offsets
;;
;; Primary Application:
;; - Developed for voluntary carbon market registries
;; - Enables transparent tracking of environmental impact claims
;; ******************************************************************

(define-constant admin-principal tx-sender) ;; Registry administrator
(define-constant error-unauthorized (err u200)) ;; Error when non-admin attempts restricted operation
(define-constant error-not-asset-holder (err u201)) ;; Error when operation attempted by non-holder
(define-constant error-duplicate-asset (err u202)) ;; Error when asset already registered
(define-constant error-missing-asset (err u203)) ;; Error when asset doesn't exist
(define-constant error-invalid-asset-data (err u204)) ;; Error for malformed asset data
(define-constant error-retirement-failed (err u205)) ;; Error when retirement process fails
(define-constant error-retirement-unauthorized (err u206)) ;; Error for unauthorized retirement
(define-constant error-group-size-invalid (err u207)) ;; Error for invalid group size
(define-constant max-group-issuance u50) ;; Maximum eco-credits in single group issuance

;; ******************************************************************
;; Registry Storage
;; ******************************************************************
(define-non-fungible-token eco-asset uint) ;; Define eco-asset NFT type
(define-data-var registry-counter uint u0) ;; Tracks the latest registered asset ID

;; ******************************************************************
;; Data Mappings
;; ******************************************************************
(define-map asset-verification-data uint (string-ascii 256)) ;; Maps asset IDs to verification data
(define-map retired-assets uint bool) ;; Registry of permanently retired assets
(define-map asset-group-data uint (string-ascii 256)) ;; Stores metadata for asset groups

;; ******************************************************************
;; Internal Helper Functions
;; ******************************************************************

;; Verify caller is the current holder of specified asset
(define-private (is-asset-holder (asset-id uint) (caller principal))
  (is-eq caller (unwrap! (nft-get-owner? eco-asset asset-id) false)))
