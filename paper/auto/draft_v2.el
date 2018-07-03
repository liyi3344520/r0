(TeX-add-style-hook
 "draft_v2"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "margin=1in") ("natbib" "authoryear")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "geometry"
    "amsmath"
    "graphicx"
    "xcolor"
    "systeme"
    "natbib"
    "tikz")
   (TeX-add-symbols
    '("com" 1)
    "XX"
    "rr")
   (LaTeX-add-labels
    "sec:intro"
    "sec:overview"
    "sec:term"
    "sec:details"
    "sec:direct"
    "sec:survival_fxn"
    "eq:r0_survivalfxn"
    "sec:direct-estim-surv"
    "sec:dpe"
    "sec:cms"
    "sec:sir-model"
    "fig::sir"
    "eq::sir"
    "eq:sir_r0"
    "least-squares-beta-gamma"
    "reparametrized-least-squares-rux5f0-gamma"
    "linear-model-approximation-degree-10"
    "linear-model-approximation-all-time-points-degree-10"
    "max-of-data"
    "smooth-maximum-of-data"
    "incidence-to-prevalence-ratio"
    "eq:harko_lin"
    "eq:r0_harko"
    "r0_attackrate"
    "sec:seqbayes"
    "sec:seir-model"
    "sec:ngm"
    "sec:exp-growth"
    "sec:expgrowth"
    "eq:lotka"
    "eq:anderson"
    "sec:mle-si"
    "eq:mlesi"
    "sec:timedep"
    "sec:igr-fs"
    "sec:network"
    "sec:contact_tracing"
    "eq:r0_contacttracing"
    "sec:branching-process"
    "sec:agent-based-models"
    "sec:methods"
    "delta-method"
    "sensitivity-analysis"
    "sec:results"
    "sec:dis"
    "fig3c"
    "hists"
    "tabs1"
    "residsf"
    "diagsf")
   (LaTeX-add-bibliographies
    "Master"))
 :latex)

