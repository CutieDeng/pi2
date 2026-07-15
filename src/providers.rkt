#lang tstring racket
;; providers.rkt — 内置命名供应商档案（profile）。
;; 主档案 lmstudio（本地 LM Studio，无需密钥）；另加各大 code-plan 云供应商：
;; openai / anthropic / gemini / grok。openai/gemini/grok 均走 OpenAI 兼容线路，
;; 仅 endpoint/model/密钥不同；anthropic 走原生 Messages 线路（provider-anthropic.rkt）。
;;
;; 档案只描述后端；切换时把 endpoint/api-key(从环境变量)/model 写进 config，
;; 而 wire 格式由注册进 host providers 表的工厂（openai vs anthropic）决定。
;; provider 运行时读 current-config → /model、/provider 切换即时生效。

(require
 (file "model.rkt")
 (file "provider.rkt")
 (file "provider-anthropic.rkt")
 (file "plugin.rkt")                    ; plugin-host-providers（struct-out 已导出）
 (file "credentials.rkt")               ; resolve-key（env > 凭据文件）
) ; end require

;; kind: 'openai | 'anthropic（线路格式）
(struct provider-profile (name kind endpoint model key-env) #:transparent)

;; 默认 model 只是切换时的起点，可再用 --model / /model 覆盖。
(define BUILTIN-PROFILES
  (list
   (provider-profile "lmstudio"  'openai    "http://localhost:1234/v1"
                     "gemma-4-31b-it@6bit" #f)
   (provider-profile "openai"    'openai    "https://api.openai.com/v1"
                     "gpt-5" "OPENAI_API_KEY")
   (provider-profile "anthropic" 'anthropic "https://api.anthropic.com"
                     "claude-sonnet-5" "ANTHROPIC_API_KEY")
   (provider-profile "gemini"    'openai    "https://generativelanguage.googleapis.com/v1beta/openai"
                     "gemini-2.0-flash" "GEMINI_API_KEY")
   (provider-profile "grok"      'openai    "https://api.x.ai/v1"
                     "grok-4" "XAI_API_KEY")
   ;; DeepSeek 提供 Anthropic 兼容端点（/anthropic/v1/messages），故走原生 anthropic 线路，
   ;; 复用 provider-anthropic.rkt 的 x-api-key 鉴权与 SSE 累加器，零新线路代码。
   (provider-profile "deepseek"  'anthropic "https://api.deepseek.com/anthropic"
                     "deepseek-chat" "DEEPSEEK_API_KEY")
  ) ; end list
) ; end define BUILTIN-PROFILES

(define (profile-by-name name)
  (findf (lambda (p) (string=? (provider-profile-name p) name)) BUILTIN-PROFILES)
) ; end define profile-by-name

(define (builtin-provider-name? name) (and (profile-by-name name) #t))

(define (builtin-provider-names) (map provider-profile-name BUILTIN-PROFILES))

;; profile → (config → provider)：线路格式按 kind 选，读 current-config 取 live 值。
(define (profile->factory p)
  (case (provider-profile-kind p)
    [(anthropic) (lambda (cfg) (make-anthropic-provider cfg))]
    [else        (lambda (cfg) (make-openai-provider cfg))]
  ) ; end case
) ; end define profile->factory

;; 把内置档案注册进 host 的 providers 表（名→工厂）。幂等；插件同名注册可覆盖。
(define (register-builtin-providers! host)
  (for ([p (in-list BUILTIN-PROFILES)])
    (hash-set! (plugin-host-providers host)
               (provider-profile-name p)
               (profile->factory p)))
) ; end define register-builtin-providers!

;; 切到某内置档案：把 endpoint/api-key(从 env)/model 写进 cfg。未知名 → 原样返回。
;; 密钥缺失时 api-key 为 #f（云端将 401；调用方可据 profile-key-env 提示）。
(define (apply-provider-profile cfg name)
  (define p (profile-by-name name))
  (if p
      (struct-copy config cfg
                   [endpoint (provider-profile-endpoint p)]
                   ;; 密钥解析：env 优先，其次 {config-home}/credentials.rktd（见 credentials.rkt）。
                   [api-key (let ([e (provider-profile-key-env p)]) (and e (resolve-key e)))]
                   [model (provider-profile-model p)])
      cfg)
) ; end define apply-provider-profile

;; profile 的密钥环境变量名（缺失提示用）；无则 #f。
(define (provider-profile-key-env-of name)
  (define p (profile-by-name name))
  (and p (provider-profile-key-env p))
) ; end define provider-profile-key-env-of

(provide
 (struct-out provider-profile)
 BUILTIN-PROFILES
 profile-by-name
 builtin-provider-name?
 builtin-provider-names
 register-builtin-providers!
 apply-provider-profile
 provider-profile-key-env-of
) ; end provide
