#' Build a forest plot by clicking
#'
#' Opens a Shiny app on a model you have already fitted, so that the exposures,
#' the modifiers and the layout can be chosen from menus rather than typed,
#' retyped and re-run. Every figure the app draws is drawn by
#' [foresty_interaction()], [foresty_main()] or [foresty_combine()], and the
#' code that drew it is shown in a tab of its own for copying back into your
#' script.
#'
#' @section What the app does not do:
#'
#' It does not fit your model. The estimates a forest plot reports come out of
#' the coefficients and the covariance matrix of a fit, so the model has to
#' exist before there is anything to draw, and which model to fit is the part of
#' the analysis that should be written down in a script rather than clicked
#' together and forgotten. The app is for the part that is fiddling --
#' [foresty_layout()] alone has some thirty arguments, and finding the figure a
#' journal wants by editing a call and re-running it is slow.
#'
#' It does not change the type of your variables either. A modifier stored as a
#' number but taking only two values is drawn as the two subgroups it is,
#' needing no change; one taking three or more is refused, and the refusal is
#' shown where the figure would have been, because turning it into a factor
#' means fitting a different model and that is a decision to make in the script,
#' not a checkbox. See the `interaction` argument of [foresty_interaction()].
#'
#' @section More than one pair:
#'
#' The exposure and the modifier menus both take as many variables as you
#' select, and what is drawn is every pair of them: three exposures and two
#' modifiers are six figures. The overall effect, which comes from the model
#' with no interaction term in it, is a checkbox of its own, so it can be drawn
#' beside the subgroups rather than instead of them. Where more than one figure
#' is drawn the app offers to combine them, which is [foresty_combine()] and
#' follows its rules: figures of one exposure become the blocks of one figure,
#' and figures of different exposures stay apart, since rows reporting the
#' effects of different exposures are not read against each other.
#'
#' How big a difference in the exposure each estimate is for -- one unit, an
#' interquartile range, an increment you name, its two ends either way round,
#' the first to the third quantile, two values you name -- is asked once for each
#' continuous exposure rather than once for all of them, since two exposures
#' rarely want the same answer. The lowest and highest values it takes are
#' written under the choice, and are where naming two values starts from.
#' Whichever was chosen is written on the figure wherever the exposure is
#' named, one unit included -- `"no2 (per 1)"`, `"no2 (per 10)"`,
#' `"no2 (per IQR, 8.44)"` -- since a reader comparing two figures across a
#' screen should not have to know that a row saying nothing means one.
#'
#' Which way round the comparison runs is written with an arrow rather than as
#' "from ... to ...", since that is the part a reader gets wrong: `Lowest value
#' -> Highest value` is the effect of being at the top of the exposure rather
#' than the bottom, and **Reverse the direction** turns it into the same
#' estimate the other way up, which is how a protective effect is drawn as one.
#' The two boxes that name two values are headed the same way, and so is the
#' figure: two values compared are written beside the exposure as
#' `"no2 (4.102 -> 38.72)"` rather than as "38.72 vs 4.102", which says which
#' two values were compared without saying which of them the estimate is the
#' effect of moving to.
#'
#' From one quantile to another is not the interquartile range, though it
#' begins at the quartiles: the range is a width, and an effect per one of it
#' is a step along a slope, whereas the quantiles are two values of the
#' exposure and are compared as `at = c(from, to)`. So it can be asked of a
#' splined exposure, which has no single slope to report a width per, and the
#' two probabilities can be set to anything -- the 10th against the 90th, the
#' median against the top decile -- rather than only 0.25 and 0.75.
#'
#' @section One reference group instead of one per subgroup:
#'
#' A subgroup figure reads each subgroup against its own reference level, so the
#' rows of one subgroup are not comparisons with the rows of another. Where the
#' exposure and the modifier both have levels there is a second question: what
#' every combination of the two comes to against one of them, which is the table
#' of groups against a baseline group a paper reports. **Model** asks it once per
#' effect modifier chosen -- *Compare every combination of the exposure and ...
#' with one group* -- and the menus under the box say which combination the rest
#' are read against: a level of the modifier, and a level of each exposure that
#' has levels.
#'
#' Every row is then that whole cell -- this level of the exposure, this level
#' of the modifier -- against the one chosen, all of it out of the same
#' interaction model, and the cell chosen is drawn as the reference it is. The
#' p-value beside the rows is the same joint test of the interaction either way:
#' it asks whether the effect of the exposure depends on the modifier, and it is
#' not a test of the rows. See the `reference` argument of
#' [foresty_interaction()].
#'
#' A continuous exposure has no levels to combine, so a pair with one is drawn
#' as it always was whatever the box says: its rows are the effect of a
#' difference in the exposure rather than groups of people.
#'
#' @section Figure style names:
#'
#' The rows, the axis and the columns of a figure are named after columns of a
#' data set, and a column is called what the data set calls it. **Figure style**
#' renames the outcome -- which is otherwise taken from the left of your
#' formula, `asthma_ever_dx` and all -- replaces the label under the plot
#' outright, and names each selected exposure. A model carrying person-time is
#' also asked what unit to count it in -- any number, 1,000 and 100,000 being
#' how a paper usually reports a rate and six figures beside the plot being
#' width the plot could have had.
#'
#' @section A model too slow to redraw:
#'
#' Every change to a control refits the model once per pair of menus, and while
#' that is happening the app looks exactly as it did before, so a line saying
#' that it is working is shown above the tabs and the figure on the screen is
#' the previous one until it goes.
#'
#' On a model where that wait is not seconds but minutes, **Write the R code
#' only** stops it: nothing is fitted and nothing is drawn, the menus and the
#' style controls go on working, and the **R code** tab goes on writing the call
#' they come to, which is what to paste into the script and run once. The other
#' tabs report on models the app fitted and say so instead.
#'
#' @section Colors and the panel:
#'
#' A figure is drawn in one color, which is asked for per exposure and can be
#' named as a place in a ColorBrewer palette -- the third color of Dark2 --
#' or typed as a hex code, which is how a figure is drawn in the color a
#' journal or a slide deck already uses. **What the colors change with** draws
#' it in more than one instead: a color per category of the rows, which is the
#' levels of a categorical exposure or the subgroups of the modifier, or a
#' color per row. Those come from a palette, or from hex codes typed beside it
#' -- `#1B9E77, #D95F02` -- which is the same list `colors` takes. See
#' `color_by` in [foresty_layout()].
#'
#' @section The tabs:
#'
#' **Plot** draws them, each figure carrying the exposure it is of under its
#' title, which is a line that can be turned off on its own where the title
#' says it already. A row of a forest plot is usually half of a comparison, and
#' the columns of counts carry the other half beside it: a row of suburban
#' children reads `822 vs 468`, those being the 822 the row is of and the 468
#' rural ones its odds ratio was estimated against. A line under the figure
#' says which two groups they are, and for a multinomial fit that the estimate
#' did not come out of the two of them alone. Nothing is said, and nothing is
#' paired, on a figure whose rows compare no two groups of people -- a
#' continuous exposure and a binary outcome. *Count each row's own group only*
#' goes back to one number a row. The line is on the screen rather than in the
#' figure, so what the download buttons write is unchanged. See *What the
#' counts beside the rows count* in [foresty_main()]. **Models** writes out, in R, how each figure was arrived at: the term
#' added to your model, the linear combination each subgroup estimate is, and
#' the test reported beside them. That code is meant to be run: it is the call
#' `foresty` made, with the same design matrix, the same coefficients and
#' covariance, the same degrees of freedom and the same test, so pasting it
#' beside the model reproduces the numbers on the figure rather than
#' approximating them. **summary(fit)** is the summary of the model you fitted
#' and of every model the app fitted from it by adding an interaction term.
#' Both tabs head each block with the outcome, the exposure and the effect
#' modifier the figure under it is of, since a coefficient table says none of
#' them. **R code** is the code twice over. *With foresty* is the code that
#' drew what is beside it, deparsed rather than reconstructed, so it cannot
#' drift from what you are looking at; several pairs are written as a loop over
#' the pairs rather than as one call apiece. It names the model with the name
#' you passed to `foresty_app()`, so pasting it into a script next to that
#' model works as it stands. *How to calculate effect estimates for each
#' subgroup* is where those numbers come from, in base R and the `car` package
#' with nothing from this package in them: the interaction term, the linear
#' combination of the coefficients each subgroup estimate is, and the joint
#' test reported beside them. It is there for a reader deciding whether to take
#' the dependency at all, and for one who would rather see what is being done
#' on their behalf than take it on trust. It comes to the estimates on the Plot
#' tab exactly -- the same design matrix, the same coefficients and covariance,
#' the same degrees of freedom and the same test -- and draws nothing: drawing
#' them is what the call above it is for. A multinomial fit is written out too:
#' it holds one equation for each level of the outcome other than the one it
#' was fitted against, so an estimate is a difference of blocks of coefficients
#' rather than a difference of two rows of the design, and the script does that
#' placing where it does the rest of the arithmetic.
#'
#' @section What comes out:
#'
#' The figure downloads as PNG and as SVG, at the size and resolution set beside
#' the buttons; where the pairs are not being combined, one file per figure
#' comes down in a zip. The reports do the same, one HTML page per model. The
#' objects themselves download as an `.rds` file holding a named list -- one
#' element per pair, plus the combined figure where there is one -- so that a
#' session that ended in the app can go on in a script: `figures <-
#' readRDS("...")` and then `summary(figures[[1]])`,
#' `as.data.frame(figures[[1]])` or `foresty_report(figures[[1]], "x.html")`.
#'
#' @param fit A fitted model, as passed to [foresty_main()] or
#'   [foresty_interaction()]. Its variables become the menus of exposures and
#'   modifiers.
#' @param measure The effect measure, as in [foresty_main()]. `NULL`, the
#'   default, takes the one the model implies.
#' @param launch Whether to start the app. `TRUE` in an interactive session.
#'   `FALSE` returns the Shiny app object without running it, which is what
#'   tests and `shiny::runApp()` want.
#' @param launch.browser Passed to [shiny::runApp()]. `TRUE`, the default,
#'   opens the app in the system browser as soon as it starts, rather than
#'   leaving a URL to be clicked or drawing it into the viewer pane of an IDE.
#'   `FALSE` starts it and says where it is, which is what a remote session or
#'   a scripted screenshot wants.
#' @param ... Passed to [shiny::runApp()], so that `port` and `host` can be
#'   set.
#'
#' @return The Shiny app object, invisibly when the app was run.
#'
#' @seealso [foresty_interaction()], [foresty_main()], [foresty_combine()],
#'   [foresty_layout()].
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
#'            family = binomial, data = foresty_cohort)
#'
#' # Opens the app in an interactive session.
#' if (interactive()) {
#'   foresty_app(fit)
#' }
#'
#' @export
foresty_app <- function(fit, measure = NULL, launch = interactive(),
                        launch.browser = TRUE, ...) {
  fy_require("shiny", "run the app")
  fy_require("bslib", "run the app")
  checkmate::assert_flag(launch.browser)

  # The name the caller knows the model by, so that the code the app writes is
  # code that runs in the caller's session rather than code mentioning a `fit`
  # that only existed inside this function.
  expr <- substitute(fit)
  fit_name <- if (is.name(expr)) as.character(expr) else "fit"

  info <- fy_model_info(fit, measure = measure)
  variables <- fy_app_variables(info)
  if (!length(variables$exposure)) {
    stop(
      "no variable of this model can be an exposure: none of them enters it as ",
      "a term of its own. Its terms are ",
      paste0("\"", names(info$term_map), "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }

  app <- shiny::shinyApp(
    ui = fy_app_ui(info, variables, fit_name),
    server = fy_app_server(fit, info, variables, fit_name, measure)
  )
  if (!isTRUE(launch)) {
    return(app)
  }
  invisible(shiny::runApp(app, launch.browser = launch.browser, ...))
}

# What each variable of the model can be asked to do.
#
# A variable can be the exposure when it enters the model as a term of its own,
# and the modifier when it has levels to split on. Which variables have levels
# is fy_check_modifier()'s decision, not a second copy of it made here, and the
# sentence it refuses with is the sentence the app shows in place of a figure:
# the guards already say what is wrong and what to do about it, so the app puts
# them in front of the choice instead of behind it.
#
# Which of them are continuous is asked as well, because the controls saying
# how big a difference in the exposure to report -- `contrast`, `at` -- mean
# nothing for a categorical one, whose comparisons are its levels. Each of
# those carries an identifier of its own, since it has controls of its own and
# two variables must not end up sharing them.
fy_app_variables <- function(info) {
  vars <- sort(setdiff(unique(unlist(info$term_vars, use.names = FALSE)),
                       info$outcome))

  exposure <- vars[vapply(vars, function(v) {
    length(fy_exposure_terms(info, v)$main) > 0L
  }, logical(1))]

  refusal <- vapply(vars, function(v) {
    out <- tryCatch({
      fy_check_modifier(info, v)
      NA_character_
    }, error = function(e) conditionMessage(e))
    out
  }, character(1))

  continuous <- vars[vapply(vars, function(v) {
    x <- tryCatch(fy_variable(info, v), error = function(e) NULL)
    !is.null(x) && is.numeric(x) && !fy_is_categorical(x)
  }, logical(1))]

  # A continuous exposure spread over several coefficients -- a spline, a
  # polynomial -- has no single effect to report, so the difference it is
  # reported for has to be two values rather than an increment. Knowing which
  # exposures those are is what lets the menu offer only the questions they can
  # answer instead of drawing an error until the reader finds the right one.
  splined <- continuous[vapply(continuous, function(v) {
    isTRUE(tryCatch(fy_is_splined(info, v), error = function(e) FALSE))
  }, logical(1))]

  # A variable with levels of its own, which is what a combination of two of
  # them can be made out of: one reference group for a whole figure is a level
  # of the exposure and a level of the modifier, and a continuous exposure has
  # neither. A flag stored as 0 and 1 is not one of these -- it is a number the
  # model fitted as a number -- so it can be the modifier, whose levels are its
  # two values, but not the exposure of such a comparison.
  categorical <- vars[vapply(vars, function(v) {
    x <- tryCatch(fy_variable(info, v), error = function(e) NULL)
    !is.null(x) && fy_is_categorical(x)
  }, logical(1))]

  ids <- stats::setNames(
    make.unique(gsub("[^A-Za-z0-9]+", "_", vars), sep = "_"),
    vars
  )

  modifier <- vars[is.na(refusal)]
  list(
    all = vars,
    exposure = exposure,
    modifier = modifier,
    continuous = continuous,
    splined = splined,
    categorical = categorical,
    # The levels of the outcome of a multinomial fit, which are the choices of
    # what every estimate is read against. Empty for a model of one equation.
    outcome_levels = info$equations$levels %||% character(0),
    outcome_reference = info$equations$reference,
    levels = fy_app_levels(info, union(modifier, categorical)),
    range = fy_app_ranges(info, continuous),
    ids = ids,
    refusal = refusal
  )
}

# The levels of each variable that has them, which is what the menus naming a
# reference group are filled from. They are taken as the figure takes them, so
# that a level offered in a menu is a level a row can be drawn for.
fy_app_levels <- function(info, vars) {
  stats::setNames(lapply(vars, function(v) {
    x <- tryCatch(fy_variable(info, v), error = function(e) NULL)
    if (is.null(x) || (is.matrix(x) && ncol(x) > 1L)) {
      return(character(0))
    }
    levels(droplevels(as.factor(x)))
  }), vars)
}

# The lowest and the highest value each continuous variable takes in the data
# the model was fitted to, which is what "from the lowest to the highest" is a
# comparison between and what the two values a reader names start at.
#
# They are rounded to four figures, so that the code the app writes says
# `at = c(4.102, 38.72)` rather than carrying fifteen decimals of a number that
# is only there to say where the range is. Four figures is closer to the
# observed value than any two people would fit a model to.
fy_app_ranges <- function(info, continuous) {
  stats::setNames(lapply(continuous, function(v) {
    x <- tryCatch(fy_variable(info, v), error = function(e) NULL)
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) < 2L || min(x) == max(x)) {
      return(NULL)
    }
    signif(range(x), 4)
  }), continuous)
}

# The menu of modifiers, in two groups. A variable that cannot be one is still
# offered, because a reader who wanted maternal age wants to be told why it is
# not there rather than to wonder where it went. The overall effect is not one
# of them: it is a checkbox, since it is not a modifier but the absence of one.
fy_app_modifier_choices <- function(variables) {
  usable <- variables$modifier
  refused <- setdiff(variables$all, usable)
  out <- list()
  if (length(usable)) {
    out[["Can be a modifier"]] <- stats::setNames(usable, usable)
  }
  if (length(refused)) {
    out[["Cannot be a modifier"]] <- stats::setNames(refused, refused)
  }
  out
}

# The columns of the table beside the plot, each said in words rather than by
# the name of its argument: a menu offering "interaction_p" tells a reader who
# already knows what it means, and nobody else.
#
# The p-value of each row is not among them. Two columns of p-values side by
# side, one testing each subgroup effect against the null and the other testing
# the interaction, are read for each other however they are headed, and the one
# a reader reaches for is the test of the interaction.
fy_app_column_choices <- function(info) {
  out <- c(
    "Effect and confidence interval" = "estimate",
    "Observations (N)" = "n"
  )
  if (!is.na(info$events)) {
    out <- c(out, "Events" = "events")
  }
  if (!is.na(info$person_time)) {
    out <- c(out, "Person-time" = "person_time")
  }
  c(out, "p for the interaction" = "interaction_p")
}

# The interface ---------------------------------------------------------------

# The arrow the controls say a direction with, by its code point so that the
# file stays plain ASCII.
fy_arrow <- "\u2192"

# A section of the sidebar that opens and shuts. There are some forty controls
# behind these panels and no more than a handful of them is wanted at once, so
# the ones that are not being used are folded away rather than scrolled past.
# Which of them start open is the accordion's business rather than the panel's,
# so a panel is only its title and what is inside it.
fy_app_panel <- function(title, ...) {
  bslib::accordion_panel(title, ...)
}

# The look is the one `ggstratify` wears, and for the same reason: the two are
# read as one pair of tools, so a reader who has used the other should not have
# to learn where anything is twice. The colors are not written here -- the
# grey title bar and the green of an open tab are Bootswatch's `flatly`, which
# `bs_theme(preset = )` fetches -- and what is left is the text Bootstrap sizes
# in pixels and the handful of classes this app has of its own.
fy_app_css <- function() {
  paste0(
    # The code tabs, and the two tabs of printed output beside them.
    "#code,#code_plain,#models,#model{background:#f6f8fa;color:#24292f;",
    "border:1px solid #d7dde3;border-radius:6px;padding:14px 16px;",
    "font-size:0.95em;line-height:1.55;max-height:70vh;overflow:auto}",
    ".accordion-button{font-weight:600}",
    # Labels, so a control is as easy to read as its value.
    ".form-label,.control-label,.shiny-input-container>label{font-weight:600}",
    # Notes, hints and alerts, all of which Bootstrap sets to 0.875em.
    ".small,small,.form-text,.help-block,.alert{font-size:0.95em}",
    # ionRangeSlider ships its own px sizes, which no theme scale reaches.
    ".irs-min,.irs-max,.irs-single,.irs-from,.irs-to{font-size:0.8em}",
    ".irs-grid-text{font-size:0.7em}",
    # The figure is drawn at the export size; if the panel is narrower the
    # page scrolls rather than squeezing the plot between fixed-width columns.
    ".fy-figure{margin-bottom:24px;overflow-x:auto}",
    ".fy-figure h4{font-weight:600;margin:4px 0 6px 0}",
    ".fy-caption{color:#5b6570;font-size:90%;margin:2px 0 0 0;max-width:46em}",
    ".fy-buttons .btn{margin:0 6px 6px 0}",
    ".fy-note{color:#5b6570;font-size:90%;margin:-4px 0 10px 0}",
    ".fy-heading{font-weight:600;margin:6px 0 2px 0}",
    ".fy-exposure{border-left:3px solid #dfe3e8;padding-left:10px;",
    "margin-bottom:12px}",
    ".fy-exposure>strong{display:block;margin-bottom:4px}",
    # What the app says while it is working. It sits above the figures rather
    # than over them, so that the figure being replaced stays readable while
    # the next one is being fitted.
    ".fy-waiting{display:flex;align-items:center;gap:10px;",
    "border:1px solid #cfd8e3;border-radius:6px;background:#eef3f9;",
    "color:#33475b;padding:10px 14px;margin-bottom:14px;font-weight:600}",
    ".fy-spinner{width:14px;height:14px;border-radius:50%;",
    "border:2px solid #b6c6d8;border-top-color:#33475b;flex:none;",
    "animation:fy-spin 0.9s linear infinite}",
    ".fy-slow-advice{display:none;margin-top:8px;font-weight:400}",
    "@keyframes fy-spin{to{transform:rotate(360deg)}}",
    # A device that would rather not see it move.
    "@media (prefers-reduced-motion:reduce){.fy-spinner{animation:none}}"
  )
}

# Copying the code happens in the browser rather than on the server, so that it
# also works when the app is being read over a remote session.
fy_app_copy_js <- function() {
  paste0(
    "$(document).on('click', '.fy-copy', function() {",
    "  var el = document.getElementById($(this).data('target'));",
    "  if (el) { navigator.clipboard.writeText(el.innerText); }",
    "});"
  )
}

# The button that does it, named for the output it copies.
fy_app_copy_button <- function(target) {
  shiny::tags$button(
    type = "button", class = "btn btn-secondary btn-sm fy-copy",
    `data-target` = target,
    "Copy to clipboard"
  )
}

# What the app says while it is fitting a model and drawing a figure.
#
# A model with an interaction term added is refitted for every pair of menus,
# and on a large one that is seconds rather than milliseconds, during which the
# app looks exactly as it did before: the old figure is still on the screen and
# nothing says that it is the old one. So Shiny's own busy flag is shown as a
# line saying so. It needs no package and no callback -- the class is on <html>
# whenever the server has work outstanding.
fy_app_waiting <- function() {
  shiny::tagList(
    shiny::tags$script(shiny::HTML("(function() {
      var timer;
      $(document).on('shiny:busy', function() {
        clearTimeout(timer); $('#fy-slow-advice').hide();
        timer = setTimeout(function() { $('#fy-slow-advice').show(); }, 10000);
      });
      $(document).on('shiny:idle', function() {
        clearTimeout(timer); $('#fy-slow-advice').hide();
      });
    })();")),
    shiny::conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      shiny::div(
        class = "fy-waiting",
        shiny::div(class = "fy-spinner"),
        shiny::div(
          "Fitting the model and drawing the figure -- waiting... Anything ",
          "already on the screen is the previous one.",
          shiny::div(
            id = "fy-slow-advice", class = "fy-slow-advice",
            "This model is taking a while. For a large data set, consider ",
            "copying the R code and running it outside Shiny."
          )
        )
      )
    )
  )
}

fy_app_ui <- function(info, variables, fit_name) {
  bslib::page_sidebar(
    title = "foresty",
    # The theme `ggstratify` wears, and deliberately the same one: the grey
    # title bar and the green of an open tab come from Bootswatch's `flatly`,
    # and `font_scale` raises Bootstrap's base size so that every control,
    # label and table grows with it. The figures are drawn by ggplot2 and are
    # unaffected -- their text size is the slider in the sidebar.
    theme = bslib::bs_theme(version = 5, preset = "flatly", font_scale = 1.35),
    # The figures are drawn at the size they export at and there may be a dozen
    # of them, so the page scrolls rather than fitting itself to the window.
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 400,
      open = "desktop",

      # The two choices the figure is of. Both keep every variable picked, so
      # that a second exposure is added to the first rather than replacing it.
      shiny::selectInput(
        "exposure", "Exposures (as many as you want)",
        choices = variables$exposure, selected = variables$exposure[1L],
        multiple = TRUE
      ),
      shiny::selectInput(
        "modifier", "Effect modifiers (as many as you want)",
        choices = fy_app_modifier_choices(variables), multiple = TRUE
      ),
      shiny::checkboxInput(
        "overall",
        "Also draw the overall effect, from the model with no interaction term",
        value = TRUE
      ),
      shiny::checkboxInput(
        "combine", "Combine the pairs into one figure", value = TRUE
      ),
      shiny::div(class = "fy-note", paste0(
        "Every exposure is drawn against every modifier.",
        "One figure per exposure and its modifiers as ",
        "blocks. Uncombined, each figure is exported as a file of its own."
      )),

      bslib::accordion(
        multiple = TRUE,
        # Everything else is folded away until it is wanted; the buttons are
        # what a reader reaches for once the figure is right, so they are the
        # one section that starts open.
        open = "Export",

        fy_app_panel(
          "Figure style",
          shiny::selectInput("style", "Journal style",
                             choices = c("classic", "jama", "nejm", "lancet",
                                         "bmj", "revman")),
          shiny::sliderInput("base_size", "Text size (pt)", min = 8, max = 36,
                             value = 16, step = 1),
          shiny::radioButtons(
            "color_by", "What the colors change with",
            choices = c("One color for the whole figure" = "none",
                        "One per category of the rows" = "category",
                        "One per row" = "row"),
            selected = "none"
          ),
          shiny::conditionalPanel(
            "input.color_by != 'none'",
            shiny::selectInput("palette", "Palette",
                               choices = c("Dark2", "Set1", "Set2")),
            shiny::textInput(
              "colors_hex", "Or hex codes of your own, separated by commas",
              placeholder = "#1B9E77, #D95F02, #7570B3"
            ),
            shiny::div(class = "fy-note", paste0(
              "The colors are taken in the order they are written and reused ",
              "from the start where the rows outrun them. A category keeps ",
              "its color wherever it appears. No legend is drawn: the rows ",
              "are labelled already."
            ))
          ),
          shiny::checkboxInput("table", "Table of numbers beside the plot",
                               value = TRUE),
          shiny::selectInput(
            "columns", "Columns of that table (empty for the style's default)",
            choices = fy_app_column_choices(info), multiple = TRUE
          ),
          shiny::checkboxInput(
            "counts_row",
            "Count each row's own group only, not the group it is compared with",
            value = FALSE
          ),
          shiny::div(class = "fy-note", paste0(
            "A row of a forest plot is half of a comparison. By default the ",
            "other half is beside it -- 822 vs 468 -- so the odds ratio on ",
            "that row can be read against the numbers it came out of. Tick ",
            "this to write the row's own group alone."
          )),
          shiny::numericInput(
            "digits",
            paste0("Decimal places for the ", tolower(info$measure_name),
                   " and its interval"),
            value = 2, min = 0, max = 4, step = 1
          ),
          shiny::checkboxInput("use_xlim", "Fix the limits of the plot",
                               value = FALSE),
          shiny::conditionalPanel(
            "input.use_xlim == true",
            shiny::numericInput("xlim_low", "Lower limit", value = 0.5),
            shiny::numericInput("xlim_high", "Upper limit", value = 2)
          ),
          shiny::textInput("title", "Title (empty for the default)"),
          shiny::checkboxInput("no_title", "Draw no title", value = FALSE),
          shiny::checkboxInput(
            "no_subtitle",
            "Draw no subtitle (the \"Exposure: ...\" line under the title)",
            value = FALSE
          ),
          # Names are part of how the finished figure looks, so keep them with
          # the other figure-wide styling choices rather than with estimation.
          # The exposure is renamed where it is chosen -- foresty_main() takes
          # `exposure = c(NO2 = "no2")` -- so there is a box per exposure, in
          # this Figure style panel. The outcome and the axis belong to the
          # whole figure and are asked once.
          shiny::div(class = "fy-note", paste0(
            "The outcome is taken from the left of your formula, which is the ",
            "name of a column and rarely what a figure should say the effect ",
            "is an effect on. Each selected exposure can be renamed here."
          )),
          shiny::fluidRow(
            shiny::column(6, shiny::textInput(
              "outcome",
              if (is.na(info$outcome %||% NA)) {
                "What to call the outcome"
              } else {
                paste0("What to call the outcome (empty for \"", info$outcome,
                       "\")")
              }
            )),
            shiny::column(6, shiny::uiOutput("exposure_labels"))
          ),
          shiny::uiOutput("modifier_labels"),
          shiny::textInput("xlab", "Label under the plot (empty for the default)",
                           placeholder = fy_axis_label(info, TRUE)),
          fy_app_person_time_control(info)
        ),

        fy_app_panel(
          "Each exposure",
          shiny::div(class = "fy-note", paste0(
            "Two exposures rarely want the same answers, so the color they ",
            "are drawn in and the difference each estimate is for are asked ",
            "once apiece."
          )),
          shiny::conditionalPanel(
            "input.color_by != 'none'",
            shiny::div(class = "fy-note", paste0(
              "The rows are being drawn in colors of their own, so the ",
              "color chosen here is not used."
            ))
          ),
          shiny::uiOutput("exposure_controls")
        ),

        fy_app_panel(
          "Model",
          shiny::radioButtons(
            "test", "Test of the interaction",
            choices = c("Likelihood ratio" = "lrt", "Wald" = "wald"),
            selected = "lrt"
          ),
          fy_app_scale_control(info),
          fy_app_outcome_reference_control(info),
          shiny::numericInput("ci_level", "Confidence level", value = 0.95,
                              min = 0.5, max = 0.9999, step = 0.01),
          shiny::uiOutput("reference_controls")
        ),

        fy_app_panel(
          "Export",
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput("width", "Width (in)",
                                                 value = 10, min = 2, step = 1)),
            shiny::column(6, shiny::numericInput("height", "Height (in)",
                                                 value = 6, min = 2, step = 1))
          ),
          shiny::numericInput("dpi", "Resolution of the PNG (dpi)", value = 300,
                              min = 72, max = 900, step = 50),
          shiny::div(
            class = "fy-buttons",
            shiny::downloadButton("save_png", "PNG"),
            shiny::downloadButton("save_svg", "SVG"),
            shiny::downloadButton("save_objects", "Objects (.rds)"),
            shiny::downloadButton("save_report", "Reports (HTML)")
          )
        )
      )
    ),

    fy_app_waiting(),
    bslib::navset_card_tab(
      id = "main_tabs",

      bslib::nav_panel(
        "Plot",
        shiny::uiOutput("message"),
        shiny::uiOutput("plots")
      ),
      bslib::nav_panel(
        "Models",
        shiny::helpText(
          "How each figure was arrived at, written out in R. The code is",
          "what foresty does, not a description of it."
        ),
        shiny::verbatimTextOutput("models")
      ),
      bslib::nav_panel(
        "summary(fit)",
        shiny::helpText(
          "The model you fitted, and every model the app fitted from it",
          "by adding an interaction term."
        ),
        shiny::verbatimTextOutput("model")
      ),
      # Both ways of writing the same figure, one under the other: the call
      # foresty made, and what that call saves. A reader who wants the package
      # takes the first; a reader who would rather not add a dependency, or
      # who wants to see what is being done on their behalf before they trust
      # it, takes the second and edits it.
      bslib::nav_panel(
        "R code",
        shiny::div(class = "fy-heading", "With foresty"),
        shiny::helpText("The code that drew the figures."),
        shiny::div(class = "fy-buttons", fy_app_copy_button("code")),
        shiny::verbatimTextOutput("code"),
        shiny::hr(),
        shiny::div(class = "fy-heading",
                   "How to calculate effect estimates for each subgroup"),
        shiny::helpText(
          "Where the numbers come from, in base R and the car package with",
          "nothing from this package in them: the interaction term, the linear",
          "combination of the coefficients each subgroup estimate is, and the",
          "joint test reported beside them. It comes to the estimates on the",
          "Plot tab exactly, and draws nothing."
        ),
        shiny::div(class = "fy-buttons", fy_app_copy_button("code_plain")),
        shiny::verbatimTextOutput("code_plain")
      )
    ),

    shiny::tags$head(shiny::tags$style(shiny::HTML(fy_app_css()))),
    shiny::tags$script(shiny::HTML(fy_app_copy_js()))
  )
}

# Which scale the estimates are reported on, said as the two things it is a
# choice between rather than as a box to tick.
#
# A mean difference and a bare coefficient are on that scale already and have
# no ratio to be drawn as, so there is nothing to choose and no control.

fy_app_scale_control <- function(info) {
  if (!isTRUE(info$exponentiate)) {
    return(shiny::div(class = "fy-note", paste0(
      "This model reports a ", tolower(info$measure_name),
      ", which is on the scale it was fitted on; there is no ratio scale to ",
      "choose between."
    )))
  }
  ratio <- info$measure_name
  shiny::radioButtons(
    "scale", "Scale the estimates are reported on",
    choices = stats::setNames(
      c("ratio", "log"),
      c(paste0(ratio, " -- the regression coefficient exponentiated, drawn ",
               "about 1"),
        paste0("Regression coefficient -- the scale the model was fitted on, ",
               "drawn about 0"))
    ),
    selected = "ratio"
  )
}

# The level of the outcome every estimate is read against, for a multinomial
# fit, which is the one choice such a model has that no other model does.
#
# Each of its equations compares one level of the outcome with the level the
# model was referred to, and which level that is was settled by how the outcome
# was coded rather than by anything about the question. Changing it here is not
# a refit: the odds ratio of one level against another is the difference
# between their two equations, and the covariance of the pair is in the model
# already. A model of one equation has no such choice and is asked nothing.
fy_app_outcome_reference_control <- function(info) {
  levels <- info$equations$levels
  if (is.null(levels) || length(levels) < 3L) {
    return(NULL)
  }
  shiny::tagList(
    shiny::selectInput(
      "outcome_reference", "Outcome level the estimates are read against",
      choices = levels, selected = info$equations$reference
    ),
    shiny::div(class = "fy-note", paste0(
      "Every other level of ", info$outcome %||% "the outcome",
      " is drawn against it, one row apiece. \"",
      info$equations$reference, "\" is the level this model was fitted ",
      "against; naming another does not refit anything."
    )),
    shiny::checkboxInput(
      "outcome_reference_row",
      "Draw that level as a row of its own", value = FALSE
    ),
    shiny::div(class = "fy-note", paste0(
      "It carries no estimate ",
      ",but it puts the level ",
      "everything is read against on the figure rather than only in the ",
      "row labels."
    ))
  )
}

# The unit person-time is counted in, for a model that carries any.
#
# A cohort of a few hundred thousand person-years puts six figures in a column
# beside the plot and takes that width from it, and a rate is reported per
# 1,000 or per 100,000 in a paper anyway. A model with no time behind it -- a
# logistic regression -- has no such column and is asked nothing.
fy_app_person_time_control <- function(info) {
  if (is.na(info$person_time)) {
    return(NULL)
  }
  shiny::tagList(
    shiny::numericInput(
      # 1 is the total and is the least it can be: person-time is divided by
      # this, and a unit of nothing is not a unit.
      "person_time", "Count person-time in units of", value = 1, min = 1,
      step = 1000
    ),
    shiny::div(class = "fy-note", paste0(
      "1 is the total, which is ", fy_format_count(info$person_time),
      " here. 1000 draws thousands of person-time and heads the column ",
      "\"Person-time (per 1,000)\"."
    ))
  )
}

# The colors an exposure can be drawn in. A figure is drawn in one color --
# the point, its border and its interval -- so this is that color rather than
# a palette, and "the style's own" leaves the journal style to say.
#
# The eight of Dark2 are offered by their number as well as by their name, since
# that is how a figure drawn to match another one is asked for: the third color
# of Dark2, not "purple". They are foresty_colors()'s own, so the code the app
# writes and the swatch beside the menu cannot come apart.
#
# The last of them is a hex code typed rather than picked, which is what a
# figure drawn to match a journal's own colors or the rest of a slide deck
# needs, since no menu can hold those.
fy_app_color_choices <- function() {
  dark2 <- foresty_colors("Dark2")
  names(dark2) <- paste0("Dark2 ", seq_along(dark2), " -- ",
                         c("green", "orange", "purple", "pink", "light green",
                           "yellow", "brown", "grey"))
  c("The style's own" = "",
    "Black" = "#000000",
    dark2,
    "Blue" = "#1A476F",
    "Red" = "#B24745",
    "A hex code I type" = "hex")
}

# A hex color as it has to be written to be one: `#RGB`, `#RRGGBB` or
# `#RRGGBBAA`, with the `#` supplied where it was left out, since that is how a
# color is copied out of a style guide. Anything else is not a color and is
# treated as though nothing had been typed, so that a half-typed code draws the
# figure in the style's own rather than refusing to draw it.
fy_app_hex <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  out <- trimws(unlist(strsplit(as.character(x), "[,;[:space:]]+")))
  out <- out[nzchar(out)]
  out <- ifelse(startsWith(out, "#"), out, paste0("#", out))
  out[grepl("^#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", out)]
}

# The controls for one exposure. Two exposures are two sets of questions -- one
# pollutant reported per interquartile range in red, an age per decade in blue
# -- so each of them is asked separately and keeps its own answers.
fy_app_exposure_ui <- function(exposure, id, continuous, input, range = NULL,
                               splined = FALSE) {
  kind <- paste0("contrast_kind_", id)
  color <- paste0("color_", id)
  # Whatever was chosen last time is what these come back as: the controls are
  # rebuilt whenever the menu of exposures changes, and an exposure that is
  # still there has not changed its mind.
  kept <- function(name, default) shiny::isolate(input[[name]]) %||% default

  # The lowest and highest values the exposure takes are what "from the lowest
  # to the highest" compares, and they are also where naming two values starts
  # from, since a reader naming them is usually moving one of the ends.
  lowest <- if (is.null(range)) 0 else range[[1L]]
  highest <- if (is.null(range)) 1 else range[[2L]]
  ends <- if (is.null(range)) {
    NULL
  } else {
    list(shiny::div(class = "fy-note", paste0(
      "In the data this model was fitted to, ", exposure, " runs from ",
      fy_trim_number(lowest), " to ", fy_trim_number(highest), "."
    )))
  }

  difference <- if (!continuous) {
    list(shiny::div(class = "fy-note", paste0(
      "Categorical, so its comparisons are its levels and there is no ",
      "difference to name."
    )))
  } else {
    # Which way round the comparison runs is the thing a reader gets wrong, and
    # "from ... to ..." spread over a line of prose is where they get it wrong,
    # so the two ends are written with an arrow between them and read at a
    # glance: lowest -> highest is the effect of being at the top rather than
    # the bottom, and the other way round is that same estimate inverted.
    # An exposure spread over several coefficients has no single effect to be
    # reported per an increment of it: how much difference a unit makes depends
    # on where along the curve it is taken. So the questions that name an
    # increment are not offered for one, and the two values it is compared
    # between are what is asked instead. Offering them and drawing an error
    # until the reader found the one that works is the alternative.
    per_increment <- c("One unit increase" = "unit",
                       "One interquartile range increase" = "iqr",
                       "An increment increase I name" = "number")
    choices <- if (splined) character(0) else per_increment
    if (!is.null(range)) {
      choices <- c(
        choices,
        # Written by its code point so that the file stays plain ASCII.
        stats::setNames(
          c("range", "quantile"),
          c(paste0("Lowest value ", fy_arrow, " Highest value"),
            paste0("First quartile ", fy_arrow, " Third quartile"))
        )
      )
    }
    choices <- c(choices, "Two values I name" = "at")
    # Two quantiles are a comparison every exposure has, so they are where a
    # splined one starts; a plain one is still reported per a unit by default.
    default <- if (!splined) {
      "unit"
    } else if ("quantile" %in% choices) {
      "quantile"
    } else {
      "at"
    }
    selected <- kept(kind, default)
    if (!selected %in% choices) {
      # The exposure was changed for one that cannot answer the question the
      # menu was left on.
      selected <- default
    }
    note <- if (!splined) {
      NULL
    } else {
      list(shiny::div(class = "fy-note", paste0(
        exposure, " enters this model through more than one coefficient -- a ",
        "spline or a polynomial -- so the difference it makes depends on ",
        "where along the curve it is taken, and there is no single effect ",
        "per unit of it. The estimate is therefore between two values of it."
      )))
    }
    c(
      note,
      list(
        shiny::selectInput(
          kind, "The difference each estimate is for",
          choices = choices, selected = selected
        ),
        shiny::conditionalPanel(
          paste0("input.", kind, " == 'number'"),
          shiny::numericInput(paste0("contrast_", id), "Effect per",
                              value = kept(paste0("contrast_", id), 10))
        ),
        shiny::conditionalPanel(
          paste0("input.", kind, " != 'at'"),
          shiny::checkboxInput(
            paste0("contrast_reverse_", id),
            "Reverse the direction", value = kept(paste0("contrast_reverse_", id), FALSE)
          )
        ),
        # The two probabilities rather than the two values: 0.25 and 0.75 are
        # the quartiles, and any other pair -- 0.1 and 0.9, 0.5 and 0.9 -- is
        # the same question asked further out.
        shiny::conditionalPanel(
          paste0("input.", kind, " == 'quantile'"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(
              paste0("quantile_low_", id), "Lower quantile",
              value = kept(paste0("quantile_low_", id), 0.25),
              min = 0, max = 1, step = 0.05
            )),
            shiny::column(6, shiny::numericInput(
              paste0("quantile_high_", id),
              paste0(fy_arrow, " Upper quantile"),
              value = kept(paste0("quantile_high_", id), 0.75),
              min = 0, max = 1, step = 0.05
            ))
          )
        ),
        # Two boxes headed "From" and "To" are read as two numbers, and which
        # of them the estimate is the effect of moving to is what a reader has
        # to know. The arrow is on the second so that the pair reads as one
        # thing: 4.102 -> 38.72.
        shiny::conditionalPanel(
          paste0("input.", kind, " == 'at'"),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(
              paste0("at_from_", id), "Value",
              value = kept(paste0("at_from_", id), lowest)
            )),
            shiny::column(6, shiny::numericInput(
              paste0("at_to_", id), paste0(fy_arrow, " To value"),
              value = kept(paste0("at_to_", id), highest)
            ))
          ),
          shiny::div(class = "fy-note", paste0(
            "The estimate is the effect of ", exposure, " being the second ",
            "rather than the first."
          ))
        )
      ),
      ends
    )
  }

  color_hex <- paste0("color_hex_", id)
  label <- paste0("label_", id)
  shiny::div(
    class = "fy-exposure",
    shiny::tags$strong(exposure),
    shiny::selectInput(color, "Color", choices = fy_app_color_choices(),
                       selected = kept(color, "")),
    shiny::conditionalPanel(
      paste0("input.", color, " == 'hex'"),
      shiny::textInput(color_hex, "Hex code",
                       value = kept(color_hex, ""),
                       placeholder = "#B24745")
    ),
    difference
  )
}

# Names shown on the finished figure are Figure style controls.  They keep the
# same input IDs as before, so a name remains in place if exposure controls are
# rebuilt after the selection changes.
fy_app_exposure_labels_ui <- function(exposure, id, input) {
  label <- paste0("label_", id)
  kept <- function(name, default) shiny::isolate(input[[name]]) %||% default
  shiny::div(
    shiny::textInput(label, paste0("What to call ", exposure, " on the figure"),
                     value = kept(label, ""), placeholder = exposure)
  )
}

# The groups of rows in an interaction figure are its modifier's levels.  They
# are names on the finished figure just like the outcome and exposure are.
fy_app_modifier_labels_ui <- function(modifier, id, info, input) {
  x <- tryCatch(fy_variable(info, modifier), error = function(e) NULL)
  levels <- levels(droplevels(as.factor(x)))
  kept <- function(name, default) shiny::isolate(input[[name]]) %||% default
  shiny::div(
    class = "fy-exposure",
    shiny::tags$strong(paste0("Subgroup names: ", modifier)),
    shiny::textInput(
      paste0("modifier_label_", id),
      paste0("What to call ", modifier, " on the figure"),
      value = kept(paste0("modifier_label_", id), ""),
      placeholder = modifier
    ),
    lapply(seq_along(levels), function(i) {
      shiny::textInput(
        paste0("level_label_", id, "_", i),
        paste0("What to call ", levels[i], " on the figure"),
        value = kept(paste0("level_label_", id, "_", i), ""),
        placeholder = levels[i]
      )
    })
  )
}

# One reference group for a whole figure, asked once per effect modifier.
#
# The ordinary subgroup figure reads each subgroup against its own reference
# level, so the rows of one subgroup are not comparisons with the rows of
# another. Where the exposure and the modifier both have levels there is a
# second question: what each combination of the two comes to against one of
# them, which is the table of groups against a baseline group a paper reports.
# Ticking the box asks that question, and the two menus under it say which
# combination the rest are read against.
#
# It is asked per modifier because the choice belongs to the modifier -- one
# figure of ecog by egfr mutation can be drawn that way and another by sex left
# as it was -- and the level of the exposure is asked once per exposure inside
# it, since a figure of two exposures against one modifier is two figures with a
# reference group apiece.
fy_app_reference_ui <- function(modifier, exposures, variables, input) {
  id <- variables$ids[[modifier]]
  usable <- intersect(exposures, variables$categorical)
  levels <- variables$levels[[modifier]] %||% character(0)
  if (is.null(id) || !length(usable) || length(levels) < 2L) {
    return(NULL)
  }
  flag <- paste0("single_ref_", id)
  kept <- function(name, default) shiny::isolate(input[[name]]) %||% default

  shiny::div(
    class = "fy-exposure",
    shiny::tags$strong(paste0("One reference group: ", modifier)),
    shiny::checkboxInput(
      flag,
      paste0("Compare every combination of the exposure and ", modifier,
             " with one group"),
      value = isTRUE(kept(flag, FALSE))
    ),
    shiny::conditionalPanel(
      paste0("input.", flag, " == true"),
      shiny::selectInput(
        paste0("ref_level_", id), paste0("The reference level of ", modifier),
        choices = levels, selected = kept(paste0("ref_level_", id), levels[[1L]])
      ),
      lapply(usable, function(exposure) {
        here <- variables$levels[[exposure]] %||% character(0)
        name <- paste0("ref_level_", id, "_", variables$ids[[exposure]])
        shiny::selectInput(
          name, paste0("The reference level of ", exposure),
          choices = here, selected = kept(name, here[[1L]])
        )
      })
    )
  )
}

# The server ------------------------------------------------------------------

fy_app_server <- function(fit, info, variables, fit_name, measure) {
  function(input, output, session) {
    ctx <- list(info = info, variables = variables, fit_name = fit_name,
                measure = measure)

    # Every pair the menus come to, and the call that draws each of them. The
    # calls are built once and then both evaluated and printed, so the code
    # shown in the R code tab is the code that drew the figures rather than a
    # description of them written separately and left to drift.
    pairs <- shiny::reactive(fy_app_pairs(input))

    output$exposure_controls <- shiny::renderUI({
      vars <- fy_app_chosen(input$exposure)
      if (!length(vars)) {
        return(shiny::div(class = "fy-note", "Choose an exposure."))
      }
      shiny::tagList(lapply(vars, function(v) {
        fy_app_exposure_ui(v, variables$ids[[v]], v %in% variables$continuous,
                           input, range = variables$range[[v]],
                           splined = v %in% variables$splined)
      }))
    })
    output$exposure_labels <- shiny::renderUI({
      vars <- fy_app_chosen(input$exposure)
      if (!length(vars)) {
        return(shiny::div(class = "fy-note", "Choose an exposure."))
      }
      shiny::tagList(lapply(vars, function(v) {
        fy_app_exposure_labels_ui(v, variables$ids[[v]], input)
      }))
    })
    output$modifier_labels <- shiny::renderUI({
      vars <- fy_app_chosen(input$modifier)
      if (!length(vars)) return(NULL)
      shiny::tagList(lapply(vars, function(v) {
        fy_app_modifier_labels_ui(v, variables$ids[[v]], info, input)
      }))
    })
    output$reference_controls <- shiny::renderUI({
      modifiers <- fy_app_chosen(input$modifier)
      exposures <- fy_app_chosen(input$exposure)
      if (!length(modifiers)) {
        return(NULL)
      }
      blocks <- Filter(Negate(is.null), lapply(modifiers, function(v) {
        fy_app_reference_ui(v, exposures, variables, input)
      }))
      if (!length(blocks)) {
        # A figure whose exposure has no levels has no combinations to draw, and
        # a reader who was looking for the box should be told why rather than
        # left to wonder where it is.
        return(shiny::div(class = "fy-note", paste0(
          "Comparing every combination with one group needs an exposure with ",
          "levels of its own, and none of the exposures chosen has any: the ",
          "rows of a continuous one are the effect of a difference in it ",
          "rather than groups of people."
        )))
      }
      shiny::tagList(blocks)
    })
    # The panel these live in is shut until it is wanted, and an output inside
    # something shut is suspended, so it is told not to be: the controls have
    # to exist for the figure to know what they say.
    shiny::outputOptions(output, "exposure_controls", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "exposure_labels", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "modifier_labels", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "reference_controls", suspendWhenHidden = FALSE)

    figures <- shiny::reactive({
      chosen <- pairs()
      object_names <- fy_app_object_names(chosen)
      many <- length(chosen) > 1L
      out <- lapply(seq_along(chosen), function(i) {
        pair <- chosen[[i]]
        state <- fy_app_evaluate(
          fy_app_call(pair, input, ctx, many = many),
          stats::setNames(list(fit), fit_name)
        )
        c(state, list(name = object_names[i],
                      label = fy_app_pair_label(pair),
                      pair = pair))
      })
      stats::setNames(out, vapply(out, function(s) s$label, character(1)))
    })

    # The combined figure, drawn from the figures themselves rather than from
    # the model again, which is what foresty_combine() takes. A pair the guards
    # refused has no figure to combine, so it is left out and its refusal is
    # shown beside the ones that worked.
    combined <- shiny::reactive({
      drawn <- Filter(function(s) !is.null(s$value), figures())
      if (!isTRUE(input$combine) || length(drawn) < 2L) {
        return(NULL)
      }
      objects <- stats::setNames(lapply(drawn, function(s) s$value),
                                 vapply(drawn, function(s) s$name, character(1)))
      call <- fy_app_combine_call(names(objects), input, ctx,
                                  lapply(drawn, function(s) s$pair))
      fy_app_evaluate(call, objects)
    })

    # What the Plot tab draws and the buttons write: the combined figure where
    # there is one, and otherwise every figure that could be drawn.
    #
    # Each figure says which exposure it is of under its own title, so there is
    # nothing to head it with here. The exception is a combination of several
    # exposures, which foresty_combine() returns as one figure apiece and which
    # could not be given a subtitle of its own, and a figure drawn with no
    # title at all, where there is nowhere inside it for the exposure to go.
    display <- shiny::reactive({
      state <- combined()
      out <- if (!is.null(state) && !is.null(state$value)) {
        fy_app_as_list(state$value, "Combined")
      } else {
        drawn <- Filter(function(s) !is.null(s$value), figures())
        stats::setNames(lapply(drawn, function(s) s$value), names(drawn))
      }
      # Each figure says which exposure it is of under its own title, so there
      # is nothing to head it with here -- unless the titles were turned off,
      # when there is nowhere inside a figure for that to go.
      attr(out, "headings") <- if (isTRUE(input$no_title) && length(out) > 1L) {
        names(out)
      } else {
        NULL
      }
      out
    })

    # The state the single-figure outputs read: the combined figure where the
    # app is showing one, and otherwise the first pair.
    figure <- shiny::reactive({
      states <- figures()
      if (!length(states)) {
        return(list(value = NULL, notes = character(0),
                    error = "choose an exposure"))
      }
      combination <- combined()
      if (!is.null(combination) && is.null(combination$error)) {
        return(combination)
      }
      states[[1L]]
    })

    output$message <- shiny::renderUI({
      states <- figures()
      named <- length(states) > 1L
      items <- list()

      if (!length(states)) {
        items <- c(items, list(shiny::div(
          class = "alert alert-warning",
          "Choose at least one exposure."
        )))
      }

      # What foresty says while it works is worth as much as what it draws --
      # that the model already carried the interaction, that the Wald test was
      # reported because the likelihood ratio test could not be -- so its
      # messages are shown rather than swallowed. A guard that refuses outright
      # carries the remedy in the error, and that is shown in place of the
      # figure rather than crashing the session.
      for (state in states) {
        prefix <- if (named) paste0(state$label, ": ") else ""
        if (!is.null(state$error)) {
          items <- c(items, list(shiny::div(
            class = "alert alert-warning",
            shiny::strong(paste0(prefix, "this figure cannot be drawn. ")),
            state$error
          )))
        }
        for (note in state$notes) {
          items <- c(items, list(shiny::div(class = "alert alert-info",
                                            paste0(prefix, note))))
        }
      }

      combination <- combined()
      if (!is.null(combination) && !is.null(combination$error)) {
        items <- c(items, list(shiny::div(
          class = "alert alert-warning",
          shiny::strong("These figures cannot be combined. "),
          combination$error
        )))
      }

      if (!length(items)) NULL else shiny::tagList(items)
    })

    # One plot output per figure, each as wide and as tall as the export is, so
    # that what is on the screen is the shape of what comes out of the buttons.
    # Height alone used to be matched; the width then followed the browser and
    # the fixed-width text columns ate the plot. A narrow panel scrolls instead.
    output$plots <- shiny::renderUI({
      figs <- display()
      shiny::req(length(figs) > 0L)
      width <- paste0(round(fy_app_number(input$width, 10) * 96), "px")
      height <- paste0(round(fy_app_number(input$height, 6) * 96), "px")
      headings <- attr(figs, "headings")
      # A screenful is a dozen; the rest are still drawn, and still go into the
      # downloads, but a page of them nobody scrolls to is not worth the wait.
      hidden <- length(figs) - fy_app_max_plots
      shown <- min(length(figs), fy_app_max_plots)

      items <- lapply(seq_len(shown), function(i) {
        shiny::div(
          class = "fy-figure",
          if (!is.null(headings)) shiny::h4(headings[i]) else NULL,
          shiny::plotOutput(paste0("plot_", i), width = width, height = height),
          fy_app_caption(figs[[i]])
        )
      })
      if (hidden > 0L) {
        items <- c(items, list(shiny::div(
          class = "alert alert-info",
          paste0(hidden, " more ",
                 if (hidden == 1L) "figure is" else "figures are",
                 " drawn but not shown. They are in the downloads; choose ",
                 "fewer exposures or modifiers to see them here.")
        )))
      }
      shiny::tagList(items)
    })

    # The renderers are made once and read the figure they are for when they
    # draw, so that changing a control redraws rather than piling up outputs.
    for (i in seq_len(fy_app_max_plots)) {
      local({
        index <- i
        output[[paste0("plot_", index)]] <- shiny::renderPlot(res = 96, {
          figs <- display()
          shiny::req(length(figs) >= index)
          plot(figs[[index]])
        })
      })
    }

    output$models <- shiny::renderText({
      fy_app_collect(fy_app_model_report(fit, fit_name, figures(), input, ctx))
    })

    output$model <- shiny::renderText({
      fy_app_collect(fy_app_summaries(fit, fit_name, figures(), input, ctx))
    })

    output$code <- shiny::renderText({
      fy_app_script(pairs(), input, ctx)
    })

    # Where the estimates come from, without the package. It reads the fitted
    # states rather than the menus alone, because the levels of the modifier and
    # the term the interaction added are properties of the model that was fitted
    # and not of what was clicked.
    output$code_plain <- shiny::renderText({
      fy_app_plain_script(figures(), input, ctx)
    })

    output$save_png <- shiny::downloadHandler(
      filename = function() fy_app_file_name(pairs(), display(), "png"),
      content = function(file) {
        shiny::req(length(display()) > 0L)
        fy_app_save_all(display(), file, "png", input)
      }
    )

    output$save_svg <- shiny::downloadHandler(
      filename = function() fy_app_file_name(pairs(), display(), "svg"),
      content = function(file) {
        shiny::req(length(display()) > 0L)
        fy_app_save_all(display(), file, "svg", input)
      }
    )

    # The figures themselves, named for the pairs they are of, so that what was
    # clicked together can be gone on with in a script.
    output$save_objects <- shiny::downloadHandler(
      filename = function() paste0(fy_app_slug(pairs()), ".rds"),
      content = function(file) {
        drawn <- Filter(function(s) !is.null(s$value), figures())
        shiny::req(length(drawn) > 0L)
        objects <- stats::setNames(lapply(drawn, function(s) s$value),
                                   names(drawn))
        combination <- combined()
        if (!is.null(combination) && !is.null(combination$value)) {
          made <- fy_app_as_list(combination$value, "Combined")
          names(made) <- if (length(made) == 1L) {
            "Combined"
          } else {
            paste0("Combined: ", names(made))
          }
          objects <- c(objects, made)
        }
        saveRDS(objects, file)
      }
    )

    # One page per figure, since a report is of one model: what a page holds --
    # the coefficient table, the joint test -- belongs to the model it came
    # from and cannot be stacked.
    output$save_report <- shiny::downloadHandler(
      filename = function() fy_app_file_name(pairs(), display(), "html"),
      content = function(file) {
        figs <- display()
        shiny::req(length(figs) > 0L)
        fy_app_write_reports(figs, file, input)
      }
    )
  }
}

# How many plot outputs the app keeps ready. Every pair of a menu of exposures
# and a menu of modifiers is a figure, and past a dozen a screen of them is not
# read anyway.
fy_app_max_plots <- 12L

# The pairs ------------------------------------------------------------------

# What was chosen, in the order it was chosen, without the blanks a menu leaves
# behind.
fy_app_chosen <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x <- unique(as.character(x[!is.na(x)]))
  x[nzchar(x)]
}

# Every exposure against every modifier, the overall effect first where it was
# asked for. Choosing no modifier at all is the overall effect whether or not
# the box is ticked, since otherwise nothing would be drawn.
fy_app_pairs <- function(input) {
  exposures <- fy_app_chosen(input$exposure)
  if (!length(exposures)) {
    return(list())
  }
  modifiers <- fy_app_chosen(input$modifier)
  overall <- isTRUE(input$overall) || !length(modifiers)

  out <- list()
  for (exposure in exposures) {
    if (overall) {
      out[[length(out) + 1L]] <- list(exposure = exposure, modifier = NULL)
    }
    for (modifier in modifiers) {
      out[[length(out) + 1L]] <- list(exposure = exposure, modifier = modifier)
    }
  }
  out
}

fy_app_pair_label <- function(pair) {
  if (is.null(pair$modifier)) {
    paste0(pair$exposure, ", overall")
  } else {
    paste0(pair$exposure, " by ", pair$modifier)
  }
}

# What each figure is called in the code the app writes and in the list it
# exports. A repeat cannot happen from the menus, but make.unique() costs
# nothing and a duplicated name would silently drop a figure.
fy_app_object_names <- function(pairs) {
  if (!length(pairs)) {
    return(character(0))
  }
  make.unique(vapply(pairs, function(pair) {
    paste0("fig_", fy_file_slug(c(pair$exposure, pair$modifier %||% "overall")))
  }, character(1)), sep = "_")
}

# The name a download is offered under: what the figure is of where it is one
# figure, and what the figures are of where they are several.
fy_app_slug <- function(pairs) {
  if (!length(pairs)) {
    return("foresty")
  }
  if (length(pairs) == 1L) {
    return(fy_file_slug(c(pairs[[1L]]$exposure,
                          pairs[[1L]]$modifier %||% "overall")))
  }
  exposures <- unique(vapply(pairs, function(p) p$exposure, character(1)))
  fy_file_slug(c(exposures, "figures"))
}

# Several figures come down as several files, in a zip; one comes down as
# itself.
fy_app_file_name <- function(pairs, figures, ext) {
  slug <- fy_app_slug(pairs)
  if (length(figures) > 1L) {
    paste0(slug, "_", ext, ".zip")
  } else {
    paste0(slug, ".", ext)
  }
}

# The calls ------------------------------------------------------------------

# The call one pair comes to.
#
# Only what was moved off its default is written, so that the code tab reads
# like code a person would write and not like a dump of every argument the
# function takes. `foresty_main()` takes a list of models and no modifier;
# `foresty_interaction()` takes one model and one.
fy_app_call <- function(pair, input, ctx, many = FALSE) {
  modifier <- pair$modifier
  fit_symbol <- as.name(ctx$fit_name)

  named <- fy_app_exposure_arg(input, pair$exposure, ctx)
  args <- list()
  if (is.null(modifier)) {
    args[[1L]] <- call("list", fit_symbol)
    args$exposure <- named
  } else {
    args[[1L]] <- fit_symbol
    args$exposure <- named
    args$interaction <- fy_app_modifier_arg(input, modifier, ctx)
  }

  if (!is.null(ctx$measure)) args$measure <- ctx$measure
  if (identical(input$scale, "log")) args$exponentiate <- FALSE
  # Written only where it was moved off the level the model was fitted against,
  # so that the code the app writes says what was chosen rather than repeating
  # the default back.
  chosen_outcome <- fy_app_outcome_reference(input, ctx)
  if (!is.null(chosen_outcome)) args$outcome_reference <- chosen_outcome
  if (fy_app_outcome_reference_row(input, ctx)) {
    args$outcome_reference_row <- TRUE
  }
  if (!fy_app_same(input$ci_level, 0.95)) args$ci_level <- input$ci_level
  args <- c(args, fy_app_contrast_args(input, pair$exposure, ctx))
  if (!is.null(modifier)) {
    level_labels <- fy_app_level_labels_arg(input, modifier, ctx)
    if (!is.null(level_labels)) args$level_labels <- level_labels
    reference <- fy_app_reference_arg(input, pair, ctx)
    if (!is.null(reference)) args$reference <- reference
  }

  # The test belongs to the interaction, and its default is the one the package
  # picks.
  if (!is.null(modifier) && !identical(input$test %||% "lrt", "lrt")) {
    args$test <- input$test
  }
  args <- c(args, fy_app_figure_args(input,
                                     fy_app_color(input, pair$exposure, ctx)))
  if (isTRUE(many)) {
    subtitle <- fy_app_subtitle(pair$exposure, input, ctx)
    if (!is.null(subtitle)) args$subtitle <- subtitle
  }

  fn <- if (is.null(modifier)) quote(foresty_main) else quote(foresty_interaction)
  as.call(c(fn, args))
}

# The outcome level chosen in the sidebar, or NULL where the model has no such
# choice or the reader has left it on the one the model was fitted against.
fy_app_outcome_reference <- function(input, ctx) {
  levels <- ctx$variables$outcome_levels
  if (!length(levels)) {
    return(NULL)
  }
  chosen <- input$outcome_reference
  if (is.null(chosen) || !nzchar(chosen) || !chosen %in% levels ||
      identical(chosen, ctx$variables$outcome_reference)) {
    return(NULL)
  }
  chosen
}

# Whether the level everything is read against is drawn as a row of its own.
# Only a model that has such a level is asked, so the box being left over from
# a model that had one cannot write the argument for a model that has not.
fy_app_outcome_reference_row <- function(input, ctx) {
  length(ctx$variables$outcome_levels) > 0L &&
    isTRUE(input$outcome_reference_row)
}

# What this exposure is called on the figure, as the argument that says it.
#
# The label is written where the exposure is chosen -- `exposure = c(NO2 =
# "no2")` -- rather than in a `labels` vector of its own, because that is one
# thing to paste into a script instead of two, and it is how the package's own
# examples name an exposure. Nothing is written where the box was left empty.
fy_app_exposure_arg <- function(input, exposure, ctx) {
  id <- ctx$variables$ids[[exposure]]
  if (is.null(id)) {
    return(exposure)
  }
  label <- input[[paste0("label_", id)]]
  if (is.null(label) || !nzchar(trimws(label))) {
    return(exposure)
  }
  stats::setNames(exposure, trimws(label))
}

# The modifier is named where its subgroup labels are edited.  Naming it in
# `interaction` lets foresty use the same name for the subgroup heading, title
# and HTML report.
fy_app_modifier_arg <- function(input, modifier, ctx) {
  id <- ctx$variables$ids[[modifier]]
  if (is.null(id)) return(modifier)
  label <- input[[paste0("modifier_label_", id)]]
  if (is.null(label) || !nzchar(trimws(label))) return(modifier)
  stats::setNames(modifier, trimws(label))
}

# The same thing as a `labels` vector, for the places that ask the package what
# a row will say rather than telling it.
fy_app_label_of <- function(input, exposure, ctx) {
  named <- fy_app_exposure_arg(input, exposure, ctx)
  if (is.null(names(named))) {
    return(NULL)
  }
  stats::setNames(names(named), unname(named))
}

# Which difference in the exposure the estimate is of, for this exposure.
# `at` names the two values and `contrast` says how far apart they are, so a
# call carries one of them and never both.
#
# One unit is written as `contrast = 1` rather than left out. It is the same
# estimate either way, but a figure that was asked for per one unit says so --
# "no2 (per 1)" -- and a figure asked for per ten says "per 10"; a reader
# comparing two of them across a screen should not have to know that a row
# saying nothing means one.
fy_app_contrast_args <- function(input, exposure, ctx) {
  variables <- ctx$variables
  id <- variables$ids[[exposure]]
  if (is.null(id) || !exposure %in% variables$continuous) {
    return(list())
  }
  # A splined exposure has no effect per an increment of it, so it starts at
  # two quantiles rather than at one unit -- a comparison every exposure has,
  # and one it can answer. What was actually chosen is used as it stands: a
  # question this exposure cannot answer is refused rather than quietly
  # replaced by a different one whose numbers would be drawn as though they
  # were what was asked for.
  default <- if (exposure %in% variables$splined) "quantile" else "unit"
  kind <- input[[paste0("contrast_kind_", id)]] %||% default
  reverse <- isTRUE(input[[paste0("contrast_reverse_", id)]])
  range <- variables$range[[exposure]]
  if (identical(kind, "at")) {
    return(list(at = call(
      "c",
      fy_app_number(input[[paste0("at_from_", id)]],
                    if (is.null(range)) 0 else range[[1L]]),
      fy_app_number(input[[paste0("at_to_", id)]],
                    if (is.null(range)) 1 else range[[2L]])
    )))
  }
  # The two ends of the exposure, which is the comparison an exposure with no
  # natural unit is often reported for -- the healthiest against the worst
  # exposed -- and which is the one comparison a splined exposure can be asked
  # for without naming two numbers. Reversed, it is the same estimate the other
  # way up, which is how a protective effect is drawn as one.
  if (identical(kind, "range")) {
    if (is.null(range)) {
      return(list())
    }
    at <- if (reverse) rev(range) else range
    return(list(at = call("c", at[[1L]], at[[2L]])))
  }
  # Two quantiles of the exposure, which is a different question from the
  # interquartile range even though it starts at the same two places: the range
  # is a width and is reported per one of it, these are two values and are
  # compared as such. A splined exposure has no width to be reported per, so
  # this is the one form of the question it can be asked.
  if (identical(kind, "quantile")) {
    at <- fy_app_quantile_values(
      ctx$info, exposure,
      c(fy_app_number(input[[paste0("quantile_low_", id)]], 0.25),
        fy_app_number(input[[paste0("quantile_high_", id)]], 0.75))
    )
    if (is.null(at)) {
      return(list())
    }
    if (reverse) at <- rev(at)
    return(list(at = call("c", at[[1L]], at[[2L]])))
  }
  if (identical(kind, "iqr")) {
    if (reverse) {
      x <- tryCatch(fy_variable(ctx$info, exposure), error = function(e) NULL)
      return(list(contrast = -stats::IQR(x, na.rm = TRUE)))
    }
    return(list(contrast = "iqr"))
  }
  value <- input[[paste0("contrast_", id)]]
  if (identical(kind, "number")) {
    value <- fy_app_number(value, 1)
    return(list(contrast = if (reverse) -value else value))
  }
  list(contrast = if (reverse) -1 else 1)
}

# The named vector supplied to `level_labels`, written only where a subgroup
# was renamed.  The level itself remains the vector name; the input is its
# display value.
fy_app_level_labels_arg <- function(input, modifier, ctx) {
  id <- ctx$variables$ids[[modifier]]
  x <- tryCatch(fy_variable(ctx$info, modifier), error = function(e) NULL)
  levels <- levels(droplevels(as.factor(x)))
  values <- vapply(seq_along(levels), function(i) {
    trimws(input[[paste0("level_label_", id, "_", i)]] %||% "")
  }, character(1))
  if (!any(nzchar(values))) return(NULL)
  values[!nzchar(values)] <- levels[!nzchar(values)]
  as.call(c(quote(c), stats::setNames(as.list(values), levels)))
}

# The one combination of the exposure and the modifier this figure's rows are
# compared with, as the argument that says it, or NULL where the box asking for
# one is unticked.
#
# The box belongs to the modifier and the level of the exposure is asked inside
# it, so a pair whose exposure has no levels -- a continuous one -- has no such
# combination and is drawn as it always was, whatever the box says. That is the
# same rule foresty_interaction() applies, and applying it here means the app
# never writes a call the package would refuse.
fy_app_reference_arg <- function(input, pair, ctx) {
  variables <- ctx$variables
  modifier <- pair$modifier
  id <- if (is.null(modifier)) NULL else variables$ids[[modifier]]
  exposure_id <- variables$ids[[pair$exposure]]
  if (is.null(id) || is.null(exposure_id) ||
      !pair$exposure %in% variables$categorical ||
      !isTRUE(input[[paste0("single_ref_", id)]])) {
    return(NULL)
  }

  levels_of <- function(v) variables$levels[[v]] %||% character(0)
  modifier_level <- input[[paste0("ref_level_", id)]] %||%
    levels_of(modifier)[1L]
  exposure_level <- input[[paste0("ref_level_", id, "_", exposure_id)]] %||%
    levels_of(pair$exposure)[1L]
  # The menus are rebuilt whenever the choice of variables changes, and for a
  # moment they hold the level of the variable they held before. A level that is
  # not one of this variable's is left out rather than written into a call that
  # would be refused, and comes back as soon as the menu catches up.
  if (!length(modifier_level) || !length(exposure_level) ||
      is.na(modifier_level) || is.na(exposure_level) ||
      !modifier_level %in% levels_of(modifier) ||
      !exposure_level %in% levels_of(pair$exposure)) {
    return(NULL)
  }

  as.call(c(quote(c), stats::setNames(
    list(exposure_level, modifier_level), c(pair$exposure, modifier)
  )))
}

# The two values a pair of quantiles comes to, in the data the model was fitted
# to, or NULL where there are no two of them.
#
# They are rounded to four figures for the same reason the two ends of the
# exposure are: the code the app writes says `at = c(4.102, 38.72)` rather than
# carrying fifteen decimals of a number that is only there to say where the
# quartile fell, and four figures is closer to it than any two people would fit
# a model to. The numbers themselves are written into the call rather than a
# quantile() of the data, so the code says which two values were compared
# without needing the data frame to be found again to say it.
#
# A probability outside [0, 1] is not one, and two quantiles that come to the
# same value -- as they do for a variable that is mostly one number -- are not a
# comparison; both are left to fall back on the plain one unit rather than drawn
# as an estimate of nothing.
fy_app_quantile_values <- function(info, exposure, probs) {
  probs <- suppressWarnings(as.numeric(probs))
  if (length(probs) != 2L || any(!is.finite(probs)) ||
      any(probs < 0) || any(probs > 1)) {
    return(NULL)
  }
  # The lower one first. A pair given the other way round would draw the effect
  # of a fall in the exposure while the boxes above it still read as a rise,
  # and "Reverse the direction" is the control that turns the figure round.
  if (probs[[1L]] >= probs[[2L]]) {
    return(NULL)
  }
  x <- tryCatch(fy_variable(info, exposure), error = function(e) NULL)
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(NULL)
  }
  at <- signif(unname(stats::quantile(x, probs = probs, na.rm = TRUE)), 4)
  if (!all(is.finite(at)) || at[[1L]] == at[[2L]]) {
    return(NULL)
  }
  at
}

# The exposure the figure is of, said under its title.
#
# A figure carries its exposure in the title it writes for itself, but a title
# can be replaced, shortened by a journal style or left off, and a reader
# looking at a screen of figures needs to know which is which. So the app says
# it once more, as a subtitle, in the words the figure itself uses -- "no2 (per
# IQR, 8.44)" -- rather than in words of its own.
#
# It is a line a figure of one exposure does not always want, since the title
# and the rows name that exposure already, so it can be turned off on its own;
# a figure with no title at all has nowhere for it to go and never carries it.
fy_app_subtitle <- function(exposure, input, ctx) {
  if (isTRUE(input$no_title) || isTRUE(input$no_subtitle)) {
    return(NULL)
  }
  paste0("Exposure: ", fy_app_exposure_label(exposure, input, ctx))
}

# The exposure as the figure names it, the comparison behind it included --
# "no2 (per IQR, 8.44)", "no2 (4.102 -> 38.72)" -- taken from the package
# rather than written again here.
fy_app_exposure_label <- function(exposure, input, ctx) {
  args <- fy_app_contrast_args(input, exposure, ctx)
  tryCatch(
    fy_exposure_display(
      ctx$info, exposure,
      contrast = args$contrast,
      at = if (is.null(args$at)) NULL else c(args$at[[2L]], args$at[[3L]]),
      labels = fy_app_label_of(input, exposure, ctx)
    ),
    error = function(e) exposure
  )
}

# The call the combined figure comes to. The figures were drawn one at a time
# and the layout was theirs; foresty_combine() draws a new figure and so needs
# to be told the same thing again, or the style chosen in the sidebar would be
# lost the moment two figures were put together.
#
# The subtitle it can be told only where the figures are of one exposure. Where
# they are of several, foresty_combine() returns a figure apiece and one
# subtitle would be wrong on all but one of them.
fy_app_combine_call <- function(object_names, input, ctx, pairs) {
  exposures <- vapply(pairs, function(p) p$exposure, character(1))
  wanted <- unique(exposures)

  # One call per exposure rather than one call for all of them. What
  # foresty_combine() would do with several exposures is exactly this -- a
  # figure apiece, since rows of different exposures are not read against each
  # other -- and doing it here means each of them can be drawn in that
  # exposure's color and told which exposure it is of.
  one <- function(exposure) {
    here <- object_names[exposures == exposure]
    if (length(here) == 1L) {
      return(as.name(here))
    }
    args <- c(lapply(here, as.name),
              fy_app_figure_args(input, fy_app_color(input, exposure, ctx)))
    subtitle <- fy_app_subtitle(exposure, input, ctx)
    if (!is.null(subtitle)) args$subtitle <- subtitle
    as.call(c(quote(foresty_combine), args))
  }

  if (length(wanted) == 1L) {
    return(one(wanted))
  }
  as.call(c(quote(list), stats::setNames(lapply(wanted, one), wanted)))
}

# The color this exposure is drawn in, or NULL for the style's own.
#
# A figure whose rows carry colors of their own has no one color to be drawn
# in, so the choice made per exposure is left out of the call rather than
# written into it and then overridden.
fy_app_color <- function(input, exposure, ctx) {
  id <- ctx$variables$ids[[exposure]]
  if (is.null(id) || !identical(input$color_by %||% "none", "none")) {
    return(NULL)
  }
  color <- input[[paste0("color_", id)]]
  if (is.null(color) || !nzchar(color)) {
    return(NULL)
  }
  if (identical(color, "hex")) {
    # A code half typed is not a color yet, and is left as though nothing had
    # been chosen rather than drawn as an error.
    typed <- fy_app_hex(input[[paste0("color_hex_", id)]])
    return(if (length(typed)) typed[[1L]] else NULL)
  }
  color
}

# What the rows are colored by, as the arguments that say it: the palette by
# name, so that the code carries the name a reader knows rather than eight hex
# codes, or the codes themselves where those were typed instead.
fy_app_color_by_args <- function(input) {
  color_by <- input$color_by %||% "none"
  if (identical(color_by, "none")) {
    return(list())
  }
  typed <- fy_app_hex(input$colors_hex)
  colors <- if (!length(typed)) {
    input$palette %||% "Dark2"
  } else if (length(typed) == 1L) {
    typed
  } else {
    as.call(c(quote(c), as.list(typed)))
  }
  list(color_by = color_by, colors = colors)
}

# What the sidebar says about how a figure looks, as arguments shared by the
# calls that draw one. The layout is the one part of it that can differ between
# the figures of one screen, since the color belongs to the exposure.
fy_app_figure_args <- function(input, color = NULL) {
  args <- c(fy_app_table_args(input), fy_app_naming_args(input))
  layout <- fy_app_layout_call(input, color)
  if (!is.null(layout)) args$layout <- layout
  c(args, fy_app_title_args(input))
}

fy_app_table_args <- function(input) {
  args <- list()
  if (!isTRUE(input$table)) args$table <- FALSE
  if (length(input$columns)) {
    args$columns <- as.call(c(quote(c), as.list(input$columns)))
  }
  args
}

# What the figure calls the outcome and the axis, and the unit its person-time
# is counted in. All three belong to the whole figure rather than to one
# exposure, and each is left out of the call where the box beside it was left
# alone.
fy_app_naming_args <- function(input) {
  args <- list()
  if (nzchar(trimws(input$outcome %||% ""))) {
    args$outcome <- trimws(input$outcome)
  }
  if (nzchar(trimws(input$xlab %||% ""))) {
    args$xlab <- trimws(input$xlab)
  }
  if (!fy_app_same(input$person_time, 1)) {
    args$person_time <- fy_app_number(input$person_time, 1)
  }
  args
}

fy_app_title_args <- function(input) {
  if (isTRUE(input$no_title)) {
    return(list(title = NA))
  }
  if (nzchar(input$title %||% "")) {
    return(list(title = input$title))
  }
  list()
}

# The layout as the shortest thing that says it: nothing at all where every
# control is where the package left it, the name of a style where that is the
# only thing that moved, and a foresty_layout() call where more did.
fy_app_layout_call <- function(input, color = NULL) {
  style <- input$style %||% "classic"
  args <- list()
  if (!is.null(input$base_size) && !is.na(input$base_size)) {
    args$base_size <- input$base_size
  }
  if (!is.null(color) && nzchar(color)) args$color <- color
  args <- c(args, fy_app_color_by_args(input))
  if (!fy_app_same(input$digits, 2)) args$digits <- input$digits
  # The counts hold both sides of a comparison unless the box says otherwise,
  # so only the box being ticked writes anything.
  if (isTRUE(input$counts_row)) args$counts <- "row"
  if (isTRUE(input$use_xlim)) {
    args$xlim <- call("c", input$xlim_low, input$xlim_high)
  }

  if (!length(args)) {
    return(if (identical(style, "classic")) NULL else style)
  }
  as.call(c(quote(foresty_layout), list(style), args))
}

# Two numbers are the same for the purpose of leaving an argument out of the
# call, an empty numericInput being NULL rather than its default.
fy_app_same <- function(x, default) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(TRUE)
  }
  isTRUE(all.equal(as.numeric(x), default))
}

fy_app_number <- function(x, default) {
  if (is.null(x) || length(x) != 1L || is.na(x)) default else as.numeric(x)
}

# The script -----------------------------------------------------------------

# The whole of what the app is doing, as a script.
#
# One pair is one call, since a loop over one thing is a way of saying it
# twice. Several are a list of the pairs and a loop over it: what varies from
# figure to figure -- the exposure, the modifier, the difference the estimate
# is for -- is in the list, and everything they have in common is written once,
# so that a seventh figure is a line added to the list rather than a call
# copied and edited. do.call() is what lets one loop cover pairs that differ in
# which arguments they carry at all.
fy_app_script <- function(pairs, input, ctx) {
  if (!length(pairs)) {
    return("")
  }
  if (length(pairs) == 1L) {
    return(paste(deparse(fy_app_call(pairs[[1L]], input, ctx)), collapse = "\n"))
  }

  fit_symbol <- ctx$fit_name
  with_modifier <- vapply(pairs, function(p) !is.null(p$modifier), logical(1))
  # A color belongs to an exposure, so where one has been chosen the layout is
  # part of what varies between the figures and moves into the list with them.
  colored <- any(vapply(pairs, function(p) {
    !is.null(fy_app_color(input, p$exposure, ctx))
  }, logical(1)))
  shared <- fy_app_shared_args(input, ctx, colored)

  main <- paste0("do.call(foresty_main, c(list(list(", fit_symbol,
                 ")), spec", shared$main, "))")
  interaction <- paste0("do.call(foresty_interaction, c(list(", fit_symbol,
                        "), spec", shared$interaction, "))")

  body <- if (all(with_modifier)) {
    fy_app_indent(interaction, 2L)
  } else if (!any(with_modifier)) {
    fy_app_indent(main, 2L)
  } else {
    c("  if (is.null(spec$interaction)) {",
      fy_app_indent(main, 4L),
      "  } else {",
      fy_app_indent(interaction, 4L),
      "  }")
  }

  naming <- if (all(with_modifier)) {
    "  paste0(spec$exposure, \" by \", spec$interaction)"
  } else if (!any(with_modifier)) {
    "  paste0(spec$exposure, \", overall\")"
  } else {
    c("  if (is.null(spec$interaction)) {",
      "    paste0(spec$exposure, \", overall\")",
      "  } else {",
      "    paste0(spec$exposure, \" by \", spec$interaction)",
      "  }")
  }

  lines <- c(
    fy_app_specs_lines(pairs, input, ctx, colored),
    "",
    "figures <- lapply(specs, function(spec) {",
    body,
    "})",
    "names(figures) <- vapply(specs, function(spec) {",
    naming,
    "}, character(1))"
  )

  if (isTRUE(input$combine)) {
    lines <- c(lines, "", fy_app_combine_lines(pairs, input, ctx))
  }
  paste(lines, collapse = "\n")
}

# The combining, as the calls it is: one per exposure, since figures of
# different exposures are not put together. Each of them names the figures it
# is of by their place in the list, which is where a reader can find them.
fy_app_combine_lines <- function(pairs, input, ctx) {
  exposures <- vapply(pairs, function(p) p$exposure, character(1))
  wanted <- unique(exposures)

  one <- function(exposure, indent) {
    at <- which(exposures == exposure)
    args <- fy_app_figure_args(input, fy_app_color(input, exposure, ctx))
    subtitle <- fy_app_subtitle(exposure, input, ctx)
    if (!is.null(subtitle)) args$subtitle <- subtitle
    if (length(at) == 1L) {
      return(fy_app_indent(paste0("figures[[", at, "]]"), indent))
    }
    figures <- if (length(at) == length(exposures)) {
      "unname(figures)"
    } else {
      paste0("unname(figures[c(", paste(at, collapse = ", "), ")])")
    }
    if (!length(args)) {
      return(fy_app_indent(paste0("do.call(foresty_combine, ", figures, ")"),
                           indent))
    }
    c(fy_app_indent(paste0("do.call(foresty_combine, c(", figures, ","), indent),
      fy_app_wrapped(as.call(c(quote(list), args)), indent + 2L),
      fy_app_indent("))", indent))
  }

  if (length(wanted) == 1L) {
    out <- one(wanted, 0L)
    out[1L] <- paste0("combined <- ", out[1L])
    return(out)
  }
  parts <- lapply(wanted, function(exposure) {
    lines <- one(exposure, 2L)
    lines[1L] <- paste0("  `", exposure, "` = ", trimws(lines[1L], "left"))
    lines
  })
  # A comma after every group but the last.
  parts <- lapply(seq_along(parts), function(i) {
    if (i == length(parts)) {
      return(parts[[i]])
    }
    n <- length(parts[[i]])
    parts[[i]][n] <- paste0(parts[[i]][n], ",")
    parts[[i]]
  })
  c("combined <- list(", unlist(parts, use.names = FALSE), ")")
}

# What every figure of the loop is drawn with, as the tail of a do.call().
# foresty_main() has no `test`, so the two branches do not share quite the same
# arguments.
fy_app_shared_args <- function(input, ctx, colored = FALSE) {
  common <- list()
  if (!is.null(ctx$measure)) common$measure <- ctx$measure
  if (identical(input$scale, "log")) common$exponentiate <- FALSE
  chosen_outcome <- fy_app_outcome_reference(input, ctx)
  if (!is.null(chosen_outcome)) common$outcome_reference <- chosen_outcome
  if (fy_app_outcome_reference_row(input, ctx)) {
    common$outcome_reference_row <- TRUE
  }
  if (!fy_app_same(input$ci_level, 0.95)) common$ci_level <- input$ci_level
  common <- c(common, if (colored) {
    c(fy_app_table_args(input), fy_app_naming_args(input))
  } else {
    fy_app_figure_args(input)
  })
  if (colored) {
    common <- c(common, fy_app_title_args(input))
  }

  test <- list()
  if (!identical(input$test %||% "lrt", "lrt")) test$test <- input$test

  written <- function(args) {
    if (!length(args)) {
      return("")
    }
    paste0(", ", fy_app_deparse1(as.call(c(quote(list), args))))
  }
  list(main = written(common), interaction = written(c(test, common)))
}

# The pairs, as the list the loop runs over: what changes from figure to
# figure and nothing else.
fy_app_specs_lines <- function(pairs, input, ctx, colored = FALSE) {
  items <- vapply(pairs, function(pair) {
    args <- list(exposure = fy_app_exposure_arg(input, pair$exposure, ctx))
    if (!is.null(pair$modifier)) {
      args$interaction <- fy_app_modifier_arg(input, pair$modifier, ctx)
    }
  args <- c(args, fy_app_contrast_args(input, pair$exposure, ctx))
  if (!is.null(pair$modifier)) {
    level_labels <- fy_app_level_labels_arg(input, pair$modifier, ctx)
    if (!is.null(level_labels)) args$level_labels <- level_labels
    reference <- fy_app_reference_arg(input, pair, ctx)
    if (!is.null(reference)) args$reference <- reference
  }
    subtitle <- fy_app_subtitle(pair$exposure, input, ctx)
    if (!is.null(subtitle)) {
      args$subtitle <- subtitle
    }
    if (colored) {
      layout <- fy_app_layout_call(input, fy_app_color(input, pair$exposure, ctx))
      if (!is.null(layout)) {
        args$layout <- layout
      }
    }
    paste0("  ", fy_app_deparse1(as.call(c(quote(list), args))))
  }, character(1))
  c("specs <- list(", paste0(items, c(rep(",", length(items) - 1L), "")), ")")
}

fy_app_indent <- function(lines, spaces) {
  # A blank line is left blank rather than being padded out to a line of
  # spaces, which is what a line of code between two blocks of code is.
  ifelse(nzchar(lines), paste0(strrep(" ", spaces), lines), lines)
}

# A call written where it will sit some way in from the margin. deparse() wraps
# to a width it measures from the left of the call, so the width it is given is
# short by however far the call has been pushed across.
fy_app_wrapped <- function(x, indent) {
  fy_app_indent(deparse(x, width.cutoff = max(20L, 78L - indent)), indent)
}

fy_app_deparse1 <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

# Where the estimates come from, without the package ------------------------

# How the number on each row of the figure is calculated, written out in base R
# and the car package.
#
# A reader deciding whether to take a dependency, and a reviewer asking what was
# actually computed, are owed the arithmetic: the model the interaction term was
# added to, the linear combination of its coefficients that each subgroup
# estimate is, and the joint test reported beside them. Written here rather than
# described, so that it can be pasted into a script and run, and so that the
# numbers it produces can be checked against the ones on the Plot tab.
#
# It reproduces the estimates exactly -- the same design matrix, the same
# coefficients and covariance, the same degrees of freedom and the same test --
# and draws nothing. Drawing them is what the call above is for.
#
# A multinomial logistic regression holds one equation per non-reference level
# of the outcome rather than one, so its contrast is a difference of blocks of
# coefficients rather than a difference of rows of the design. That is written
# out too, by the same helpers with the block arithmetic in them.
fy_app_plain_script <- function(states, input, ctx) {
  if (!length(states)) {
    return("")
  }
  body <- unlist(lapply(seq_along(states), function(i) {
    fy_app_plain_block(states[[i]], i, input, ctx)
  }), use.names = FALSE)
  # Which of the two tests the figures below came to is read off the code that
  # takes them rather than worked out a second time, so that the helper the
  # script defines cannot be the one it does not go on to call. A function
  # defined and never used is a function to read past.
  used <- c(
    lrt = any(grepl("lrt_p(", body, fixed = TRUE)),
    wald = any(grepl("wald_p(", body, fixed = TRUE))
  )
  paste(c(fy_app_plain_preamble(input, ctx, used), body), collapse = "\n")
}

# A paragraph of the script's own explanation, wrapped to the width the tab is
# read at. The names of the outcome's levels are written into it, and a level
# named at length would otherwise run off the side of it.
fy_app_plain_note <- function(...) {
  paste0("# ", strwrap(paste0(...), width = 76))
}

# What the p-value beside the figures is, named rather than left as "the test".
# Which of the two it is turns on the model as much as on the sidebar -- a fit
# with no likelihood has the Wald test reported beside it whatever was asked
# for -- so it is named from the code that takes it.
fy_app_plain_test_name <- function(used) {
  if (used[["lrt"]] && used[["wald"]]) {
    return("the tests of the interaction")
  }
  if (used[["wald"]]) {
    return("the joint Wald test of the interaction")
  }
  "the likelihood ratio test of the interaction"
}

# What a fit of several equations adds to the arithmetic, said above the script
# that does it rather than left for the reader to work out of the code.
fy_app_plain_equations_note <- function(equations) {
  if (is.null(equations)) {
    return(character(0))
  }
  c(
    "#",
    fy_app_plain_note(
      "This is a multinomial logistic regression of ",
      length(equations$levels), " outcome levels, so it holds ",
      length(equations$blocks), " equations -- one for each level other than \"",
      equations$reference, "\", which each of them is measured against -- and ",
      "a block of coefficients for each equation, all built from the same ",
      "design matrix."
    ),
    "#",
    fy_app_plain_note(
      "The estimate on any one row is therefore not the difference between two ",
      "rows of that design matrix, which is what it is for a model of one ",
      "equation. It is that difference placed in the block of the equation the ",
      "row belongs to, less the same difference in the block of the equation ",
      "the row is read against, and zero everywhere else."
    ),
    "#",
    fy_app_plain_note(
      "The level the model was fitted against has no block of its own -- it is ",
      "the zero the other equations are measured from -- so it drops out of ",
      "that difference on its own, and a comparison between two levels neither ",
      "of which it is is the difference of their two blocks, with the ",
      "covariance of the pair already in vcov(). Reading the figure against ",
      "another level of the outcome is therefore not a refit."
    )
  )
}

# The heading, the helpers, and the handful of settings every estimate below is
# taken at, written once because every estimate is taken at the same ones.
fy_app_plain_preamble <- function(input, ctx, used) {
  info <- ctx$info
  equations <- info$equations
  exponentiate <- isTRUE(info$exponentiate) && !identical(input$scale, "log")
  c(
    strrep("#", 77),
    "# How the effect estimate in each subgroup is calculated.",
    "#",
    fy_app_plain_note(
      "Needs the car package", if (!is.null(equations)) " and nnet",
      if (inherits(info$fit, "merMod")) " and lme4",
      ", and nothing else. Everything below is base R and car: the model the ",
      "interaction term was added to, the linear combination of its ",
      "coefficients each subgroup estimate is",
      if (any(used)) {
        paste0(", and ", fy_app_plain_test_name(used), " reported beside them")
      },
      "."
    ),
    "#",
    "# Each subgroup estimate comes out of that one model rather than out of a",
    "# model fitted in each subgroup, which is what lets every subgroup share",
    "# one estimate of the covariate effects and the interaction be tested.",
    fy_app_plain_equations_note(equations),
    "#",
    "# The numbers are the ones on the Plot tab exactly: the same design matrix,",
    "# the same coefficients and covariance, the same degrees of freedom and the",
    "# same test. Nothing here draws a figure.",
    strrep("#", 77),
    "",
    "library(car)",
    if (!is.null(equations)) "library(nnet)",
    "",
    fy_app_plain_helpers(info, equations, used),
    "",
    paste0("# --- What the sidebar came to ", strrep("-", 47)),
    "",
    paste0("ci_level     <- ", fy_app_number(input$ci_level, 0.95)),
    paste0("exponentiate <- ", if (exponentiate) "TRUE" else "FALSE",
           "   # ", if (exponentiate) {
             paste0("the ", tolower(info$measure_name), ", drawn about 1")
           } else {
             "the regression coefficient, drawn about 0"
           }),
    paste0("use_t        <- ", if (is.finite(info$error_df)) "TRUE" else "FALSE",
           "   # ", if (is.finite(info$error_df)) {
             "a gaussian model: t and F on its residual degrees of freedom"
           } else {
             "the normal approximation: z and a Wald chi-square"
           }),
    "",
    "# One complete observation of the data. Every estimate is a difference",
    "# between two copies of this row, so the covariates it carries cancel; it",
    "# is here because a design matrix has to be built from a whole row.",
    fy_app_plain_at_line(info, ctx$fit_name),
    "",
    fy_app_plain_outcomes(input, ctx)
  )
}

# The comparisons between outcome levels the rows of a multinomial figure are
# made of: every level except the one they are all read against, one apiece,
# and that level itself where the sidebar asked for it as a row of its own.
#
# They belong to the model rather than to any one figure, as the scale and the
# confidence level do, so they are written once above every figure below.
fy_app_plain_outcomes <- function(input, ctx) {
  equations <- ctx$info$equations
  if (is.null(equations)) {
    return(character(0))
  }
  reference <- fy_app_outcome_reference(input, ctx) %||% equations$reference
  wanted <- setdiff(equations$levels, reference)

  item <- function(level, label, is_reference) {
    c(
      paste0("  list(label = ", fy_app_quote(label), ", level = ",
             fy_app_quote(level), ","),
      paste0("       against = ", fy_app_quote(reference), ", reference = ",
             if (is_reference) "TRUE" else "FALSE", ")")
    )
  }
  items <- lapply(wanted, function(lv) {
    item(lv, paste0(lv, " vs ", reference), FALSE)
  })
  # The level everything is read against comes first, as the reference level of
  # a categorical exposure does, and carries no estimate: it is the definition
  # the rows around it are differences from.
  if (fy_app_outcome_reference_row(input, ctx)) {
    items <- c(list(item(reference, reference, TRUE)), items)
  }
  # A comma after every item but the last, so the list parses.
  items <- lapply(seq_along(items), function(i) {
    lines <- items[[i]]
    if (i < length(items)) {
      lines[length(lines)] <- paste0(lines[length(lines)], ",")
    }
    lines
  })

  c(
    "# Which two levels of the outcome each row of the figure compares.",
    "outcomes <- list(",
    unlist(items, use.names = FALSE),
    ")",
    ""
  )
}

# The row the contrasts are taken at, named as the reader's own data rather
# than as a copy of it where the model's call says where it came from.
fy_app_plain_at_line <- function(info, fit_name) {
  where <- tryCatch(fy_reference_index(info), error = function(e) NULL)
  data_name <- fy_app_data_name(info$fit)
  taken <- if (is.null(where)) {
    paste0("model.frame(", fit_name, ")[1, ]")
  } else if (where$from_data && !is.null(data_name)) {
    paste0(data_name, "[", where$index, ", ]")
  } else {
    paste0("model.frame(", fit_name, ")[", where$index, ", ]")
  }
  paste0("at <- ", taken)
}

# What a script has to put back before it can update the fit.
#
# A model fitted inside a loop or a function keeps the symbol its call was
# written with -- `geeglm(..., corstr = cs)` -- rather than the value that
# symbol stood for, and the frame the value lived in went with the loop. The
# fit recorded what the argument came to, so the symbol is written back from
# the fit itself, which is what foresty does before it refits.
fy_app_lost_arg_lines <- function(fit, fit_name) {
  # What the script needs is what the session it is pasted into can find, not
  # what foresty could find where the formula was written: a fit made in an
  # lapply() keeps that frame alive through its formula, and update() called
  # from a script is not in it.
  lost <- fy_lost_args(fit, env = globalenv())
  if (!length(lost)) {
    return(NULL)
  }
  c(
    fy_app_plain_note(
      "The call ", fit_name, " was fitted by still names ",
      paste0("`", names(lost), "`", collapse = " and "),
      ", which the loop or the function it was fitted inside took away with ",
      "it. update() has to find ", if (length(lost) > 1L) "them" else "it",
      " again, and the fit itself recorded what ",
      if (length(lost) > 1L) "they came" else "it came", " to."
    ),
    paste0(names(lost), " <- ", fit_name, "$", lost),
    ""
  )
}

# The three functions foresty is, as functions of your own.
fy_app_plain_helpers <- function(info, equations = NULL, used) {
  if (!is.null(equations)) {
    return(fy_app_plain_multi_helpers(used))
  }
  fit <- info$fit
  c(
paste0("# --- What foresty does for you ", strrep("-", 45)),
"",
"# One estimate: the effect of the exposure as a linear combination of the",
"# coefficients of one model. The two rows differ in what is being compared and",
"# in nothing else -- the exposure, and the modifier where the comparison runs",
"# across subgroups as well -- so the difference between their rows of the",
"# design matrix is the contrast the estimate is of. Everything else is held at",
"# the same value in both rows, so what those variables contribute is the same",
"# in both and cancels from the difference; which value they are held at is",
"# therefore irrelevant. Going back through model.frame() with the model's own",
"# levels and contrast coding is what rebuilds a basis such as ns(x, 3) with",
"# the knots it was fitted with, rather than recomputing one from two rows.",
"#",
"# car::linearHypothesis.default() is called rather than the generic: the",
"# method a fit dispatches to has its own idea of which coefficients, which",
"# covariance and how many degrees of freedom to use. The hypothesis is a",
"# numeric matrix rather than a character formula because a coefficient may be",
"# named anything a fitting function likes, which car's parser reads only some",
"# of.",
"#",
"# What comes back is the ordinary Wald arithmetic -- L %*% b for the estimate",
"# and L %*% V %*% t(L) for its variance -- so the same two numbers can be had",
"# without car at all. They are taken through car here because that is the call",
"# foresty makes, and the point of this script is to be that call rather than a",
"# second implementation of it that could agree with it or not.",
"effect_of <- function(model, at, exposure, from, to, modifier = NULL,",
"                      level = NULL, from_level = level, to_level = level,",
"                      ci_level = 0.95, exponentiate = TRUE, error_df = Inf) {",
"  rows <- at[c(1, 1), , drop = FALSE]",
"  rows[[exposure]] <- c(from, to)",
"  if (!is.null(modifier)) rows[[modifier]] <- c(from_level, to_level)",
"",
"  tt <- delete.response(terms(model))",
    fy_app_frame_lines(fit, "model", "  "),
"",
    paste0("  b <- ", fy_app_coef_expr(fit, "model")),
"  b <- b[!is.na(b)]        # an aliased coefficient has no column in vcov()",
"  V <- vcov(model)[names(b), names(b), drop = FALSE]",
"  # The contrast is matched to the coefficients by name. A coefficient with no",
"  # column of that name in the design would otherwise become an NA and be",
"  # carried into the estimate without a word.",
"  stopifnot(all(names(b) %in% colnames(X)))",
"  L <- matrix((X[2, ] - X[1, ])[names(b)], nrow = 1,",
"              dimnames = list(NULL, names(b)))",
"",
"  lh <- car::linearHypothesis.default(",
"    model, L, coef. = b, vcov. = V, error.df = error_df,",
"    test = if (is.finite(error_df)) \"F\" else \"Chisq\", singular.ok = TRUE",
"  )",
"  # The estimate is the value car attaches to its result and its standard",
"  # error the square root of the variance beside it.",
"  estimate <- as.vector(attr(lh, \"value\"))",
"  se <- sqrt(as.vector(attr(lh, \"vcov\")))",
"  crit <- if (is.finite(error_df)) {",
"    qt(1 - (1 - ci_level) / 2, df = error_df)",
"  } else {",
"    qnorm(1 - (1 - ci_level) / 2)",
"  }",
"  out <- data.frame(",
"    estimate  = estimate,",
"    conf.low  = estimate - crit * se,",
"    conf.high = estimate + crit * se,",
"    p.value   = lh[[grep(\"^Pr\\\\(\", names(lh))]][2]",
"  )",
"  # The interval is formed on the scale the model was fitted on and only then",
"  # transformed, so that it stays consistent with its point estimate.",
"  if (exponentiate) {",
"    out[c(\"estimate\", \"conf.low\", \"conf.high\")] <-",
"      exp(out[c(\"estimate\", \"conf.low\", \"conf.high\")])",
"  }",
"  out",
"}",
"",
"# Which distribution an estimate is referred to. Taken from the model in hand,",
"# because the model carrying the interaction has spent degrees of freedom on",
"# it that the model without it still has.",
"error_df_of <- function(model, use_t) {",
"  if (!use_t) return(Inf)",
"  df <- tryCatch(df.residual(model), error = function(e) NULL)",
"  if (is.null(df) || is.na(df) || df <= 0) Inf else as.numeric(df)",
"}",
    if (used[["wald"]]) c(
"",
"# The p-value for the interaction, the Wald way: the joint test that every",
"# coefficient the interaction term added is zero.",
"wald_p <- function(model, columns, error_df = Inf) {",
    paste0("  b <- ", fy_app_coef_expr(fit, "model")),
"  b <- b[!is.na(b)]",
"  columns <- intersect(columns, names(b))",
"  if (!length(columns)) return(NA_real_)",
"  V <- vcov(model)[names(b), names(b), drop = FALSE]",
"  L <- matrix(0, length(columns), length(b),",
"              dimnames = list(columns, names(b)))",
"  L[cbind(seq_along(columns), match(columns, names(b)))] <- 1",
"",
"  lh <- car::linearHypothesis.default(",
"    model, L, coef. = b, vcov. = V, error.df = error_df,",
"    test = if (is.finite(error_df)) \"F\" else \"Chisq\", singular.ok = TRUE",
"  )",
"  lh[[grep(\"^Pr\\\\(\", names(lh))]][2]",
"}"
    ),
    if (used[["lrt"]]) c(
"",
"# The same hypothesis tested against the likelihood rather than against the",
"# Wald approximation to it: twice the difference in log-likelihood between the",
"# model carrying the interaction and the same model without it, referred to a",
"# chi-square on the degrees of freedom between the two. It tests the",
"# interaction as a whole rather than any one coefficient of it.",
"#",
"# The two are different asymptotic tests of the same hypothesis and usually",
"# agree. Where the Wald approximation is poor -- a small study, a rare",
"# outcome, a sparse subgroup, an estimate far from the null -- the likelihood",
"# ratio test is usually the more reliable of the two.",
"lrt_p <- function(full, reduced) {",
"  statistic <- 2 * (as.numeric(logLik(full)) - as.numeric(logLik(reduced)))",
"  df <- attr(logLik(full), \"df\") - attr(logLik(reduced), \"df\")",
"  if (is.na(df) || df <= 0 || statistic < 0) return(NA_real_)",
"  pchisq(statistic, df = df, lower.tail = FALSE)",
"}"
    )
  )
}

# The same three functions for a model of several equations.
#
# They are written out in full rather than folded into the ones above with a
# branch in them, because a reader is being shown the arithmetic of the model
# in front of them and a branch for a model they have not fitted is one more
# thing to read past. What differs is where the contrast goes: the design is
# one equation wide, the coefficients are as wide as all of them together, and
# equation_row() is what puts the one in the other.
fy_app_plain_multi_helpers <- function(used) {
  c(
paste0("# --- What foresty does for you ", strrep("-", 45)),
"",
"# The coefficients of this model as one vector, named as its covariance",
"# matrix is. coef() gives a matrix -- one row per equation, one column per",
"# term -- and vcov() a matrix named \"level:term\", so the vector is that",
"# matrix read row by row and named the same way.",
"#",
"# The names are rebuilt here rather than read off vcov(), so that the two are",
"# lined up by name below and not by the order they happen to come in. A name",
"# built here that vcov() does not carry stops the subset that follows rather",
"# than quietly matching the wrong coefficient.",
"flat_coef <- function(model) {",
"  b <- coef(model)",
"  if (!is.matrix(b)) return(b[!is.na(b)])",
"  setNames(as.vector(t(b)),",
"           paste(rep(rownames(b), each = ncol(b)),",
"                 rep(colnames(b), nrow(b)), sep = \":\"))",
"}",
"",
"# The difference between the two rows being compared, in the space of one",
"# equation. The two rows differ in what is being compared and in nothing else",
"# -- the exposure, and the modifier where the comparison runs across subgroups",
"# as well. Everything else is held at the same value in both rows, so what",
"# those variables contribute is the same in both and cancels from the",
"# difference. Going back through model.frame() with the model's own levels and",
"# contrast coding is what rebuilds a basis such as ns(x, 3) with the knots it",
"# was fitted with, rather than recomputing one from two rows. Every equation",
"# is built from this same design, so the difference is taken once and placed",
"# in the blocks below.",
"design_difference <- function(model, at, exposure, from, to, modifier = NULL,",
"                              from_level = NULL, to_level = NULL) {",
"  rows <- at[c(1, 1), , drop = FALSE]",
"  rows[[exposure]] <- c(from, to)",
"  if (!is.null(modifier)) rows[[modifier]] <- c(from_level, to_level)",
"",
"  tt <- delete.response(terms(model))",
"  mf <- model.frame(tt, data = rows, xlev = model$xlevels,",
"                    na.action = na.pass)",
"  X  <- model.matrix(tt, data = mf, contrasts.arg = model$contrasts)",
"  X[2, ] - X[1, ]",
"}",
"",
"# One row of the contrast matrix: that difference placed in the block of the",
"# equation the row is of, less the same difference in the block of the",
"# equation it is read against, and zero everywhere else. The level the model",
"# was fitted against has no block -- it is the zero the other equations are",
"# measured from -- so it drops out of the difference on its own.",
"equation_row <- function(b, base, dx, outcome, against) {",
"  L <- setNames(numeric(length(b)), names(b))",
"  # The design is one equation wide and the coefficients are as wide as all",
"  # of them together, so the two are matched by name. A design column the",
"  # coefficients do not name would otherwise become an NA and be carried into",
"  # the estimate without a word.",
"  stopifnot(all(base %in% names(dx)))",
"  dx <- dx[base]",
"  place <- function(level, sign) {",
"    nm <- paste(level, base, sep = \":\")",
"    hit <- nm %in% names(L)",
"    L[nm[hit]] <<- L[nm[hit]] + sign * dx[hit]",
"  }",
"  place(outcome, 1)",
"  place(against, -1)",
"  matrix(L, nrow = 1, dimnames = list(NULL, names(L)))",
"}",
"",
"# One estimate: the effect of the exposure on the odds of one level of the",
"# outcome against another, as a linear combination of the coefficients of one",
"# model.",
"#",
"# car::linearHypothesis.default() is called rather than the generic: the",
"# method a fit dispatches to has its own idea of which coefficients, which",
"# covariance and how many degrees of freedom to use. The hypothesis is a",
"# numeric matrix rather than a character formula because a coefficient may be",
"# named anything a fitting function likes, which car's parser reads only some",
"# of.",
"#",
"# What comes back is the ordinary Wald arithmetic -- L %*% b for the estimate",
"# and L %*% V %*% t(L) for its variance -- so the same two numbers can be had",
"# without car at all. They are taken through car here because that is the call",
"# foresty makes, and the point of this script is to be that call rather than a",
"# second implementation of it that could agree with it or not.",
"effect_of <- function(model, at, exposure, from, to, outcome, against,",
"                      modifier = NULL, level = NULL, from_level = level,",
"                      to_level = level, ci_level = 0.95, exponentiate = TRUE,",
"                      error_df = Inf) {",
"  b <- flat_coef(model)",
"  V <- vcov(model)[names(b), names(b), drop = FALSE]",
"  dx <- design_difference(model, at, exposure, from, to, modifier,",
"                          from_level, to_level)",
"  L <- equation_row(b, colnames(coef(model)), dx, outcome, against)",
"",
"  lh <- car::linearHypothesis.default(",
"    model, L, coef. = b, vcov. = V, error.df = error_df,",
"    test = if (is.finite(error_df)) \"F\" else \"Chisq\", singular.ok = TRUE",
"  )",
"  # The estimate is the value car attaches to its result and its standard",
"  # error the square root of the variance beside it.",
"  estimate <- as.vector(attr(lh, \"value\"))",
"  se <- sqrt(as.vector(attr(lh, \"vcov\")))",
"  crit <- if (is.finite(error_df)) {",
"    qt(1 - (1 - ci_level) / 2, df = error_df)",
"  } else {",
"    qnorm(1 - (1 - ci_level) / 2)",
"  }",
"  out <- data.frame(",
"    estimate  = estimate,",
"    conf.low  = estimate - crit * se,",
"    conf.high = estimate + crit * se,",
"    p.value   = lh[[grep(\"^Pr\\\\(\", names(lh))]][2]",
"  )",
"  # The interval is formed on the scale the model was fitted on and only then",
"  # transformed, so that it stays consistent with its point estimate.",
"  if (exponentiate) {",
"    out[c(\"estimate\", \"conf.low\", \"conf.high\")] <-",
"      exp(out[c(\"estimate\", \"conf.low\", \"conf.high\")])",
"  }",
"  out",
"}",
"",
"# The label a row is drawn under: what the row is of, and which two levels of",
"# the outcome it compares. The level the others are read against is one row",
"# for the whole of the exposure, so it is labelled by the subgroup it sits in",
"# rather than by a value of the exposure it is not about.",
"label_of <- function(oc, of, within = NULL) {",
"  parts <- c(if (isTRUE(oc$reference)) within else of, oc$label)",
"  paste(parts, collapse = \", \")",
"}",
"",
"# Which distribution an estimate is referred to. A multinomial fit is referred",
"# to the normal, so this is Inf for it, and it is written out anyway because",
"# what it answers is a property of the model rather than of the script.",
"error_df_of <- function(model, use_t) {",
"  if (!use_t) return(Inf)",
"  df <- tryCatch(df.residual(model), error = function(e) NULL)",
"  if (is.null(df) || is.na(df) || df <= 0) Inf else as.numeric(df)",
"}",
    if (used[["wald"]]) c(
"",
"# The p-value for the interaction, the Wald way: the joint test that every",
"# coefficient the interaction term added is zero. It added one to each",
"# equation, so the test spans all of them and spends a degree of freedom on",
"# each, which is what makes it a test of whether the effect depends on the",
"# modifier at all rather than one test for each level of the outcome.",
"wald_p <- function(model, columns, error_df = Inf) {",
"  b <- flat_coef(model)",
"  columns <- intersect(columns, names(b))",
"  if (!length(columns)) return(NA_real_)",
"  V <- vcov(model)[names(b), names(b), drop = FALSE]",
"  L <- matrix(0, length(columns), length(b),",
"              dimnames = list(columns, names(b)))",
"  L[cbind(seq_along(columns), match(columns, names(b)))] <- 1",
"",
"  lh <- car::linearHypothesis.default(",
"    model, L, coef. = b, vcov. = V, error.df = error_df,",
"    test = if (is.finite(error_df)) \"F\" else \"Chisq\", singular.ok = TRUE",
"  )",
"  lh[[grep(\"^Pr\\\\(\", names(lh))]][2]",
"}"
    ),
    if (used[["lrt"]]) c(
"",
"# The same hypothesis tested against the likelihood rather than against the",
"# Wald approximation to it: twice the difference in log-likelihood between the",
"# model carrying the interaction and the same model without it, referred to a",
"# chi-square on the degrees of freedom between the two. It tests the",
"# interaction as a whole, across every equation the model holds, and so spends",
"# the same degrees of freedom the Wald test above does.",
"#",
"# The two are different asymptotic tests of the same hypothesis and usually",
"# agree. Where the Wald approximation is poor -- a small study, a rare",
"# outcome, a sparse subgroup, an estimate far from the null -- the likelihood",
"# ratio test is usually the more reliable of the two.",
"lrt_p <- function(full, reduced) {",
"  statistic <- 2 * (as.numeric(logLik(full)) - as.numeric(logLik(reduced)))",
"  df <- attr(logLik(full), \"df\") - attr(logLik(reduced), \"df\")",
"  if (is.na(df) || df <= 0 || statistic < 0) return(NA_real_)",
"  pchisq(statistic, df = df, lower.tail = FALSE)",
"}"
    )
  )
}

# One figure's worth of estimates: the model they come out of, the rows they
# are, and the test reported beside them.
fy_app_plain_block <- function(state, index, input, ctx) {
  pair <- state$pair
  exposure <- pair$exposure
  modifier <- pair$modifier
  suffix <- paste0("_", index)

  comparisons <- fy_app_plain_comparisons(input, exposure, ctx)
  if (is.null(comparisons)) {
    return(c(
      paste0("# --- Figure ", index, ": ", state$label, " ", strrep("-", 40)),
      paste0("# Not written: which two values of \"", exposure,
             "\" are being compared could"),
      "# not be worked out, so there is nothing to contrast.",
      ""
    ))
  }

  opening <- paste0("# --- Figure ", index, ": ", state$label, " ")
  head <- c(paste0(opening, strrep("-", max(3L, 78L - nchar(opening)))), "")
  reference <- fy_app_plain_reference(state, input, ctx)
  body <- if (is.null(modifier)) {
    fy_app_plain_overall(state, suffix, input, ctx, comparisons)
  } else if (is.null(reference)) {
    fy_app_plain_subgroups(state, suffix, input, ctx, comparisons)
  } else {
    fy_app_plain_cells(state, suffix, input, ctx, reference)
  }
  c(head, body, "")
}

# The loop that fills in a figure's rows, wrapped in one over the comparisons
# between levels of the outcome.
#
# A fit of one equation has one comparison to make, which is the exposure
# against itself elsewhere. A multinomial fit repeats that comparison within
# each comparison of the outcome, and the rows come out in that order -- the
# outcome outside, the exposure inside -- which is the order the figure draws
# them in.
#
# `reference_row` is the level everything is read against, drawn as a row of
# its own. It is one row for the whole of the exposure, since it is the same
# definition at every value of it, so only the first value is drawn for it.
# Where it was not asked for there is no such row and nothing is written about
# it: a branch that can never be taken is a line to read past.
fy_app_plain_over_outcomes <- function(inner, iterated, reference_row) {
  c(
    "do.call(rbind, lapply(outcomes, function(oc) {",
    if (reference_row) {
      c(paste0("  drawn <- ", iterated),
        "  # The level everything is read against is one row for the whole of",
        "  # the exposure: it is the same definition at every value of it.",
        "  if (isTRUE(oc$reference)) drawn <- drawn[1]")
    },
    fy_app_indent(inner, 2),
    "}))"
  )
}

# Whether a row is a definition rather than an estimate, as the script has to
# ask it: the reference level of a categorical exposure, and, where the level
# of the outcome the others are read against is drawn as a row of its own,
# that row too.
fy_app_plain_definition <- function(reference_row,
                                    of = "isTRUE(cmp$reference)") {
  if (reference_row) paste0("isTRUE(oc$reference) || ", of) else of
}

# The overall effect: one model, and one row per comparison the exposure is of.
fy_app_plain_overall <- function(state, suffix, input, ctx, comparisons) {
  exposure <- state$pair$exposure
  fit_name <- ctx$fit_name
  label <- fy_app_exposure_label(exposure, input, ctx)
  multi <- !is.null(ctx$info$equations)
  outcome_row <- multi && fy_app_outcome_reference_row(input, ctx)

  inner <- c(
    paste0("do.call(rbind, lapply(",
           if (outcome_row) "drawn" else paste0("comparisons", suffix),
           ", function(cmp) {"),
    paste0("  out <- if (", fy_app_plain_definition(outcome_row), ") {"),
    "    data.frame(estimate = if (exponentiate) 1 else 0, conf.low = NA_real_,",
    "               conf.high = NA_real_, p.value = NA_real_)",
    "  } else {",
    paste0("    effect_of(", fit_name, ", at, \"", exposure,
           "\", cmp$from, cmp$to,"),
    if (multi) "              outcome = oc$level, against = oc$against,",
    "              ci_level = ci_level, exponentiate = exponentiate,",
    paste0("              error_df = error_df_of(", fit_name, ", use_t))"),
    "  }",
    if (multi) {
      c(paste0("  of <- if (is.na(cmp$label)) ", fy_app_quote(label),
               " else cmp$label"),
        "  cbind(label = label_of(oc, of), out)")
    } else {
      paste0("  cbind(label = if (is.na(cmp$label)) ",
             fy_app_quote(label), " else cmp$label, out)")
    },
    "}))"
  )
  if (multi) {
    inner <- fy_app_plain_over_outcomes(inner, paste0("comparisons", suffix),
                                        outcome_row)
  }

  c(
    paste0("comparisons", suffix, " <- list("),
    paste0(comparisons, c(rep(",", length(comparisons) - 1L), "")),
    ")",
    "",
    paste0("rows", suffix, " <- ", inner[1L]),
    inner[-1L],
    paste0("rownames(rows", suffix, ") <- NULL"),
    paste0("rows", suffix)
  )
}

# One estimate per level of the modifier, all of them out of the one model that
# carries the interaction term. The modifier is held at that level in both rows
# of the contrast, so each row is the effect of the exposure inside one subgroup
# and the subgroups are not compared with each other.
fy_app_plain_subgroups <- function(state, suffix, input, ctx, comparisons) {
  exposure <- state$pair$exposure
  modifier <- state$pair$modifier
  fit_name <- ctx$fit_name
  int_name <- paste0("fit_int", suffix)
  levels <- fy_app_plain_levels(input, modifier, ctx)
  added <- fy_app_plain_added(state, exposure, modifier, ctx)
  multi <- !is.null(ctx$info$equations)
  outcome_row <- multi && fy_app_outcome_reference_row(input, ctx)

  inner <- c(
    paste0("do.call(rbind, lapply(",
           if (outcome_row) "drawn" else paste0("comparisons", suffix),
           ", function(cmp) {"),
    paste0("  out <- if (", fy_app_plain_definition(outcome_row), ") {"),
    "    data.frame(estimate = if (exponentiate) 1 else 0,",
    "               conf.low = NA_real_, conf.high = NA_real_,",
    "               p.value = NA_real_)",
    "  } else {",
    paste0("    effect_of(", int_name, ", at, \"", exposure,
           "\", cmp$from, cmp$to,"),
    if (multi) "              outcome = oc$level, against = oc$against,",
    paste0("              modifier = \"", modifier, "\", level = levels",
           suffix, "[i],"),
    "              ci_level = ci_level, exponentiate = exponentiate,",
    paste0("              error_df = error_df_of(", int_name, ", use_t))"),
    "  }",
    paste0("  label <- if (is.na(cmp$label)) level_labels", suffix,
           "[i] else"),
    paste0("    paste0(level_labels", suffix, "[i], \": \", cmp$label)"),
    if (multi) {
      paste0("  cbind(label = label_of(oc, label, level_labels", suffix,
             "[i]), out)")
    } else {
      "  cbind(label = label, out)"
    },
    "}))"
  )
  if (multi) {
    inner <- fy_app_plain_over_outcomes(inner, paste0("comparisons", suffix),
                                        outcome_row)
  }

  c(
    fy_app_lost_arg_lines(ctx$info$fit, fit_name),
    "# The model carrying the interaction.",
    paste0(int_name, " <- update(", fit_name, ", . ~ . + ",
           paste(added, collapse = " + "), ")"),
    "",
    paste0("levels", suffix, "       <- c(", levels$values, ")"),
    paste0("level_labels", suffix, " <- c(",
           paste(fy_app_quote(levels$labels), collapse = ", "), ")"),
    paste0("comparisons", suffix, "  <- list("),
    paste0(comparisons, c(rep(",", length(comparisons) - 1L), "")),
    ")",
    "",
    if (multi) {
      "# One row per subgroup, per comparison of the outcome within it, and per"
    } else {
      "# One row per subgroup, and per comparison within it."
    },
    if (multi) "# comparison of the exposure within that.",
    paste0("rows", suffix, " <- do.call(rbind, lapply(seq_along(levels",
           suffix, "), function(i) {"),
    fy_app_indent(inner, 2),
    "}))",
    paste0("rownames(rows", suffix, ") <- NULL"),
    paste0("rows", suffix),
    "",
    fy_app_plain_test_said(state, input),
    fy_app_plain_test(state, suffix, int_name, exposure, modifier, added,
                      input, ctx),
    fy_app_plain_said(suffix)
  )
}

# Every combination of the exposure and the modifier against one of them.
#
# The two rows of the contrast now differ in both variables -- the reference
# combination against this one -- so what each row reports is a whole group
# against one chosen group rather than the effect of the exposure inside a
# subgroup. The test beside them is the same test of the same coefficients: it
# asks whether the effect of the exposure depends on the modifier, and it is not
# a test of the rows.
fy_app_plain_cells <- function(state, suffix, input, ctx, reference) {
  exposure <- state$pair$exposure
  modifier <- state$pair$modifier
  fit_name <- ctx$fit_name
  int_name <- paste0("fit_int", suffix)
  levels <- fy_app_plain_levels(input, modifier, ctx)
  added <- fy_app_plain_added(state, exposure, modifier, ctx)
  exposure_levels <- fy_app_plain_exposure_levels(input, exposure, ctx)
  multi <- !is.null(ctx$info$equations)
  outcome_row <- multi && fy_app_outcome_reference_row(input, ctx)

  inner <- c(
    paste0("do.call(rbind, lapply(",
           if (outcome_row) "drawn" else paste0("exposure_levels", suffix),
           ", function(lv) {"),
    paste0("  same <- lv == ref_exposure", suffix, " && levels", suffix,
           "[i] == ref_modifier", suffix),
    paste0("  out <- if (", fy_app_plain_definition(outcome_row, "same"), ") {"),
    "    data.frame(estimate = if (exponentiate) 1 else 0,",
    "               conf.low = NA_real_, conf.high = NA_real_,",
    "               p.value = NA_real_)",
    "  } else {",
    paste0("    effect_of(", int_name, ", at, \"", exposure, "\","),
    if (multi) "              outcome = oc$level, against = oc$against,",
    paste0("              from = ref_exposure", suffix, ", to = lv,"),
    paste0("              modifier = \"", modifier, "\","),
    paste0("              from_level = ref_modifier", suffix,
           ", to_level = levels", suffix, "[i],"),
    "              ci_level = ci_level, exponentiate = exponentiate,",
    paste0("              error_df = error_df_of(", int_name, ", use_t))"),
    "  }",
    if (multi) {
      c(paste0("  of <- paste0(level_labels", suffix, "[i], \": \", lv)"),
        paste0("  cbind(label = label_of(oc, of, level_labels", suffix,
               "[i]), out)"))
    } else {
      paste0("  cbind(label = paste0(level_labels", suffix,
             "[i], \": \", lv), out)")
    },
    "}))"
  )
  if (multi) {
    inner <- fy_app_plain_over_outcomes(inner,
                                        paste0("exposure_levels", suffix),
                                        outcome_row)
  }

  c(
    fy_app_lost_arg_lines(ctx$info$fit, fit_name),
    "# The model carrying the interaction.",
    paste0(int_name, " <- update(", fit_name, ", . ~ . + ",
           paste(added, collapse = " + "), ")"),
    "",
    paste0("levels", suffix, "          <- c(", levels$values, ")"),
    paste0("level_labels", suffix, "    <- c(",
           paste(fy_app_quote(levels$labels), collapse = ", "), ")"),
    paste0("exposure_levels", suffix, " <- c(",
           paste(fy_app_quote(exposure_levels), collapse = ", "), ")"),
    "",
    "# The one combination every row below is compared with.",
    paste0("ref_exposure", suffix, "    <- ",
           fy_app_quote(reference$exposure_level)),
    paste0("ref_modifier", suffix, "    <- ",
           fy_app_level_values(ctx$info, modifier, reference$modifier_level)),
    "",
    "# One row per combination of the two, each against that one.",
    paste0("rows", suffix, " <- do.call(rbind, lapply(seq_along(levels",
           suffix, "), function(i) {"),
    fy_app_indent(inner, 2),
    "}))",
    paste0("rownames(rows", suffix, ") <- NULL"),
    paste0("rows", suffix),
    "",
    fy_app_plain_test_said(state, input),
    "# It is the test of the interaction rather than a test of any row.",
    fy_app_plain_test(state, suffix, int_name, exposure, modifier, added,
                      input, ctx),
    fy_app_plain_said(suffix)
  )
}

# The line that says what the test came to, since a p-value assigned to a
# variable and never printed is a p-value nobody sees.
fy_app_plain_said <- function(suffix) {
  paste0("cat(\"p for the interaction:\", signif(p_interaction", suffix,
         ", 3), \"\\n\")")
}

# Which of the two tests this figure reported, as it was drawn rather than as
# the menus were read.
#
# The sidebar asks for one, and the model has the last word: a fit with no
# likelihood -- a GEE, which is estimating equations rather than a likelihood
# -- has the joint Wald test reported beside it however the box is set, and so
# does a fit whose standard errors are robust. A script writing the other one
# would print a p-value that is not the one on the figure, and for a GEE would
# not run at all.
fy_app_plain_is_lrt <- function(state, input) {
  drawn <- if (is.null(state$value)) {
    NULL
  } else {
    tryCatch(fy_result(state$value)$interaction_test$test,
             error = function(e) NULL)
  }
  if (!is.null(drawn) && !is.na(drawn)) {
    return(grepl("[Ll]ikelihood", drawn))
  }
  identical(input$test %||% "lrt", "lrt")
}

# The line above it that says which of the two it is, since a p-value whose
# test is not named is a p-value a reader has to guess at.
fy_app_plain_test_said <- function(state, input) {
  if (fy_app_plain_is_lrt(state, input)) {
    "# The p-value reported beside them: the likelihood ratio test."
  } else {
    "# The p-value reported beside them: the joint Wald test."
  }
}

# The test that was taken, written as the arithmetic it is.
fy_app_plain_test <- function(state, suffix, int_name, exposure, modifier,
                              added, input, ctx) {
  if (fy_app_plain_is_lrt(state, input)) {
    return(c(
      paste0("p_interaction", suffix, " <- lrt_p("),
      paste0("  ", int_name, ","),
      paste0("  update(", int_name, ", . ~ . - ", paste(added, collapse = " - "),
             ")"),
      ")"
    ))
  }
  columns <- fy_app_plain_columns(state, exposure, modifier)
  named <- fy_app_plain_column_lines(columns)
  named[length(named)] <- paste0(named[length(named)], ",")
  c(
    paste0("p_interaction", suffix, " <- wald_p("),
    paste0("  ", int_name, ","),
    named,
    paste0("  error_df = error_df_of(", int_name, ", use_t)"),
    ")"
  )
}

# The one combination this figure's rows are compared with, as the figure was
# drawn rather than as the menus were read: a pair the package refused the
# reference for was drawn the ordinary way, and the script beside it says so.
fy_app_plain_reference <- function(state, input, ctx) {
  if (is.null(state$value)) {
    return(NULL)
  }
  tryCatch(fy_result(state$value)$reference_cell, error = function(e) NULL)
}

# The levels of the exposure, in the order the figure draws them.
fy_app_plain_exposure_levels <- function(input, exposure, ctx) {
  known <- ctx$variables$levels[[exposure]]
  if (!is.null(known) && length(known)) {
    return(known)
  }
  x <- tryCatch(fy_variable(ctx$info, exposure), error = function(e) NULL)
  levels(droplevels(as.factor(x)))
}

# The comparisons the exposure is of, as the list the loop runs over. A
# continuous exposure has one; a categorical one has its levels, the first of
# them the reference the others are read against.
fy_app_plain_comparisons <- function(input, exposure, ctx) {
  args <- fy_app_contrast_args(input, exposure, ctx)
  values <- tryCatch(
    fy_exposure_values(
      ctx$info, exposure,
      contrast = args$contrast,
      at = if (is.null(args$at)) NULL else c(args$at[[2L]], args$at[[3L]])
    ),
    error = function(e) NULL
  )
  if (!length(values)) {
    return(NULL)
  }
  vapply(values, function(v) {
    level <- v$level %||% NA_character_
    paste0("  list(label = ",
           if (is.na(level)) "NA_character_" else fy_app_quote(level),
           ", from = ", fy_app_value_text(v$from),
           ", to = ", fy_app_value_text(v$to),
           ", reference = ", if (isTRUE(v$reference)) "TRUE" else "FALSE", ")")
  }, character(1))
}

# The levels of the modifier: the values to put back into the data, which have
# to be of the type the column is, and the names the figure calls them by.
fy_app_plain_levels <- function(input, modifier, ctx) {
  x <- tryCatch(fy_variable(ctx$info, modifier), error = function(e) NULL)
  levels <- levels(droplevels(as.factor(x)))
  id <- ctx$variables$ids[[modifier]]
  labels <- vapply(seq_along(levels), function(i) {
    given <- trimws(input[[paste0("level_label_", id, "_", i)]] %||% "")
    if (nzchar(given)) given else levels[i]
  }, character(1))
  list(levels = levels, labels = labels,
       values = fy_app_level_values(ctx$info, modifier, levels))
}

# The term the interaction added, as the model that was fitted has it, so that
# `update()` in the script adds what the app added.
fy_app_plain_added <- function(state, exposure, modifier, ctx) {
  fallback <- paste0(exposure, ":", modifier)
  if (is.null(state$value)) {
    return(fallback)
  }
  info <- tryCatch(fy_infos(state$value)[[1L]], error = function(e) NULL)
  if (is.null(info)) {
    return(fallback)
  }
  added <- setdiff(fy_app_term_labels(info$fit),
                   fy_app_term_labels(ctx$info$fit))
  if (!length(added)) fallback else added
}

# Those coefficients as the script writes them: on one line where they fit,
# and one to a line where they do not. A multi-equation fit names a coefficient
# after its equation as well as after its term, and there is one of them per
# equation, so the list runs off the side of the tab often enough to matter.
fy_app_plain_column_lines <- function(columns, indent = 2L) {
  quoted <- fy_app_quote(columns)
  pad <- strrep(" ", indent)
  one <- paste0(pad, "c(", paste(quoted, collapse = ", "), ")")
  # One character in hand for the comma the caller adds to the last line.
  if (nchar(one) <= 77L) {
    return(one)
  }
  c(
    paste0(pad, "c("),
    paste0(pad, "  ", quoted, c(rep(",", length(quoted) - 1L), "")),
    paste0(pad, ")")
  )
}

# The coefficients the Wald test is taken over.
fy_app_plain_columns <- function(state, exposure, modifier) {
  fallback <- paste0(exposure, ":", modifier)
  if (is.null(state$value)) {
    return(fallback)
  }
  info <- tryCatch(fy_infos(state$value)[[1L]], error = function(e) NULL)
  if (is.null(info)) {
    return(fallback)
  }
  columns <- tryCatch(
    fy_modifier_interaction_columns(info, exposure, modifier),
    error = function(e) character(0)
  )
  if (!length(columns)) fallback else columns
}

# A string as the script has to write it, quotes and backslashes escaped.
fy_app_quote <- function(x) {
  paste0("\"", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", x)), "\"")
}

# What the counts beside the rows are counts of, written under the figure.
#
# A figure of a continuous exposure and a binary outcome needs nothing said
# about them, and nothing is said: the note is drawn only for the figures whose
# columns of counts do not mean what a reader takes them to mean, which is a
# categorical exposure, whose rows count one level apiece, and a multinomial
# fit, whose rows count everyone whatever their outcome and whose events column
# is of the level the row is of rather than of the level it is compared with.
#
# It goes under the plot on the screen rather than into the plot, so that what
# the buttons write is the figure a paper takes and not a figure carrying a
# note to its reader about how to read a screen.
fy_app_caption <- function(figure) {
  note <- fy_result(figure)$counts_note
  if (!length(note)) {
    return(NULL)
  }
  shiny::div(class = "fy-caption", paste(note, collapse = " "))
}

# Drawing --------------------------------------------------------------------

# Evaluate one of the app's calls, keeping what it said while it worked.
#
# The objects the call names are put in an environment of their own, so that a
# call mentioning `my_fit` or `fig_no2_by_sex` is the call a reader can paste
# into a script beside those objects.
fy_app_evaluate <- function(call, objects) {
  env <- new.env(parent = environment())
  for (nm in names(objects)) {
    assign(nm, objects[[nm]], envir = env)
  }

  notes <- character(0)
  out <- tryCatch(
    withCallingHandlers(
      list(value = eval(call, envir = env), error = NULL),
      message = function(m) {
        notes <<- c(notes, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) list(value = NULL, error = conditionMessage(e))
  )
  out$notes <- notes
  out$call <- call
  out
}

# A figure, or the several that foresty_combine() returns when the exposures
# differ, as a named list of figures to draw.
fy_app_as_list <- function(x, single_name) {
  # A figure is a ggplot and a ggplot is a list, so what it is comes first.
  if (inherits(x, "foresty")) {
    return(stats::setNames(list(x), single_name))
  }
  if (is.list(x)) {
    out <- unclass(x)
    if (is.null(names(out))) {
      names(out) <- paste(single_name, seq_along(out))
    }
    return(out)
  }
  stats::setNames(list(x), single_name)
}

# A figure as the plot it is, without the class that carries its estimates:
# patchwork is being asked to stack these, and what it hands back would carry
# the attribute of whichever figure it took it from.
fy_app_plain <- function(x) {
  class(x) <- setdiff(class(x), "foresty")
  attr(x, "foresty") <- NULL
  x
}

# The text a tab comes to, collected rather than written ----------------------

# The Models and summary(fit) tabs are panels of text, and text has to be built
# somewhere. cat() would build it on the console, which is neither where the
# app puts it nor somewhere a caller of this package asked for anything to be
# written. So it is collected instead: each function below adds what it has to
# say to the buffer fy_app_collect() opened, and fy_app_collect() hands back
# the one string the panel is given.
fy_app_text <- new.env(parent = emptyenv())

fy_app_collect <- function(expr) {
  old <- fy_app_text$lines
  on.exit(fy_app_text$lines <- old, add = TRUE)
  fy_app_text$lines <- character(0)
  force(expr)
  paste0(fy_app_text$lines, collapse = "")
}

# cat()'s arguments, into that buffer rather than to the console: each of them
# as it would be shown, joined by `sep`.
fy_app_say <- function(..., sep = " ") {
  pieces <- unlist(lapply(list(...), as.character), use.names = FALSE)
  fy_app_text$lines <- c(fy_app_text$lines, paste0(pieces, collapse = sep))
  invisible(NULL)
}

# An object as its own print method writes it. A summary of a model is worth
# having on the tab in the form summary() gives it, and capture.output() is how
# that text is had without any of it being written anywhere.
fy_app_say_printed <- function(x) {
  fy_app_say(paste0(utils::capture.output(print(x)), collapse = "\n"), "\n",
             sep = "")
}


# What was fitted, and how the numbers were arrived at ------------------------

# The summary tab: the model you fitted, and every model the app fitted from it.
#
# The overall figure comes from the first and each subgroup figure from one of
# the others, and the difference between them is the whole of what a subgroup
# analysis is, so both are here rather than only the one you typed.
fy_app_summaries <- function(fit, fit_name, states, input = NULL, ctx = NULL) {
  fy_app_say(strrep("=", 72), "\n", sep = "")
  fy_app_say("THE MODEL YOU FITTED -- no interaction term\n")
  fy_app_say("The overall effect, where it is drawn, is drawn from this one.\n")
  fy_app_say(strrep("=", 72), "\n", sep = "")
  fy_app_say_facts(NULL, input, ctx)
  fy_app_say("\n")
  fy_app_say_printed(summary(fit))

  seen <- character(0)
  for (state in states) {
    if (is.null(state$value) || is.null(state$pair$modifier)) {
      next
    }
    formula <- fy_app_formula_text(fy_infos(state$value)[[1L]]$fit)
    if (formula %in% seen) {
      next
    }
    seen <- c(seen, formula)
    fy_app_say("\n\n", strrep("=", 72), "\n", sep = "")
    fy_app_say("WITH THE ", state$pair$exposure, " BY ", state$pair$modifier,
        " INTERACTION TERM\n", sep = "")
    fy_app_say("The subgroup effects of ", state$pair$exposure,
        " are drawn from this one.\n", sep = "")
    fy_app_say(strrep("=", 72), "\n", sep = "")
    fy_app_say_facts(state, input, ctx)
    fy_app_say("\n")
    fy_app_say_printed(summary(fy_infos(state$value)[[1L]]$fit))
  }
  invisible(NULL)
}

# What the figure under a heading is of, said in the three things a reader has
# to know to read it: what the estimate is an estimate of, what it is the
# effect of, and what it was split by. A summary of a model says none of them
# -- a coefficient table is a list of terms -- and neither does a block of code,
# so they are written above both.
fy_app_say_facts <- function(state, input, ctx, indent = "  ") {
  if (is.null(ctx)) {
    return(invisible(NULL))
  }
  exposure <- if (is.null(state)) NULL else state$pair$exposure
  said <- if (is.null(exposure)) {
    "every exposure drawn, one figure apiece"
  } else {
    fy_app_exposure_label(exposure, input, ctx)
  }
  modifier <- if (is.null(state)) {
    "none -- this model carries no interaction term"
  } else if (is.null(state$pair$modifier)) {
    "none -- the overall effect"
  } else {
    state$pair$modifier
  }
  fy_app_say(indent, "Outcome:         ", ctx$info$outcome %||% "not named", "\n",
      sep = "")
  fy_app_say(indent, "Exposure:        ", said, "\n", sep = "")
  fy_app_say(indent, "Effect modifier: ", modifier, "\n", sep = "")
  # Which cell the rows are read against, where they are read against one cell
  # rather than each against its own subgroup. A row means a different thing in
  # the two figures, and a tab of coefficients does not say which it is.
  reference <- fy_app_plain_reference(state, input, ctx)
  if (!is.null(reference)) {
    fy_app_say(indent, "Reference group: ", state$pair$exposure, " = ",
        reference$exposure_level, " and ", state$pair$modifier, " = ",
        reference$modifier_level, "\n", sep = "")
  }
  invisible(NULL)
}

# The Models tab: what foresty did, written out as the R it amounts to.
#
# A subgroup analysis is read differently depending on which model it came out
# of and how the subgroup effects were taken from it, and neither of those is
# visible in the figure. The code here is not run -- the app runs foresty --
# but it is what foresty runs, named for your variables, so that a reader can
# check the analysis rather than take it on trust.
fy_app_model_report <- function(fit, fit_name, states, input, ctx) {
  base_terms <- fy_app_term_labels(fit)

  fy_app_say(strrep("=", 72), "\n", sep = "")
  fy_app_say("THE MODEL YOU FITTED -- no interaction term\n")
  fy_app_say(strrep("=", 72), "\n\n", sep = "")
  # What the model is, in the words a methods section uses. The classes of the
  # object it is held in -- "glm, lm" -- name the function that fitted it rather
  # than the model, and a reader checking the tab against a paper is looking for
  # "logistic regression".
  fy_app_say("  ", fy_model_name(fit), "\n\n", sep = "")
  call <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  if (!is.null(call)) {
    fy_app_say("  ", fit_name, " <- ",
        paste(deparse(call),
              collapse = paste0("\n", strrep(" ", nchar(fit_name) + 6L))),
        "\n\n", sep = "")
  }
  fy_app_say("  Outcome: ", ctx$info$outcome %||% "not named", "\n", sep = "")
  fy_app_say("  Formula: ", fy_app_formula_text(fit), "\n\n", sep = "")

  if (!length(states)) {
    fy_app_say("Choose an exposure to see how its figure would be arrived at.\n")
    return(invisible(NULL))
  }

  for (state in states) {
    # The model this section's figure came out of, which is the one that was
    # fitted where the section is a subgroup analysis and yours where it is not.
    # A section that could not be drawn has none, and falls back on yours.
    section_fit <- if (is.null(state$value)) {
      fit
    } else {
      fy_infos(state$value)[[1L]]$fit
    }
    fy_app_say(strrep("-", 72), "\n", sep = "")
    fy_app_say(toupper(state$label), "\n", sep = "")
    fy_app_say(strrep("-", 72), "\n", sep = "")
    fy_app_say("  ", fy_model_name(section_fit), "\n\n", sep = "")
    fy_app_say_facts(state, input, ctx)
    # The formula of that model, section by section, since it is the one thing
    # that tells a subgroup section apart from the one above it.
    fy_app_say("  Formula:         ", fy_app_formula_text(section_fit), "\n", sep = "")
    fy_app_say("\n")
    if (is.null(state$value)) {
      fy_app_say("  Not drawn: ", state$error, "\n\n", sep = "")
      next
    }
    if (is.null(state$pair$modifier)) {
      fy_app_report_overall(state, fit_name, input, ctx)
    } else {
      fy_app_report_interaction(state, fit, fit_name, base_terms, input, ctx)
    }
  }

  fy_app_say(strrep("=", 72), "\n", sep = "")
  fy_app_say("A note on the code above\n")
  fy_app_say(strrep("=", 72), "\n\n", sep = "")
  fy_app_say("  It is meant to be run. It is what foresty ran, named for your model\n")
  fy_app_say("  and your variables: the same two rows of data, the same design\n")
  fy_app_say("  matrix, the same coefficients and covariance, the same degrees of\n")
  fy_app_say("  freedom and the same test, so that pasting it beside the model\n")
  fy_app_say("  reproduces the numbers on the figure rather than something close to\n")
  fy_app_say("  them.\n\n")
  fy_app_say("  It calls car::linearHypothesis.default() rather than\n")
  fy_app_say("  car::linearHypothesis(), and passes the hypothesis as a numeric\n")
  fy_app_say("  matrix rather than as a character formula. The method a fit would\n")
  fy_app_say("  dispatch to has its own idea of which coefficients, which covariance\n")
  fy_app_say("  and how many degrees of freedom to use, and would quietly use the\n")
  fy_app_say("  model's own where a robust variance was asked for; and a coefficient\n")
  fy_app_say("  may be named anything a fitting function likes, which car's formula\n")
  fy_app_say("  parser reads only some of.\n")
  invisible(NULL)
}

# The overall effect: the code it is, and nothing about it.
#
# What the code does is what the code says -- two rows of the design matrix
# differing in the exposure alone, a linear combination of the coefficients,
# a Wald test of it -- and a paragraph saying the same thing above it is read
# once and skipped every time after.
fy_app_report_overall <- function(state, fit_name, input, ctx) {
  result <- fy_result(state$value)
  exposure <- result$exposure
  info <- fy_infos(state$value)[[1L]]
  compared <- fy_app_compared(exposure, input, ctx)

  fy_app_say_code(fy_app_lincom_code(info, fit_name, exposure, compared), 6L)
}

fy_app_report_interaction <- function(state, fit, fit_name, base_terms, input,
                                      ctx) {
  result <- fy_result(state$value)
  exposure <- result$exposure
  modifier <- result$modifier
  info <- fy_infos(state$value)[[1L]]
  added <- setdiff(fy_app_term_labels(info$fit), base_terms)
  levels <- unique(as.character(result$estimates$modifier_level))
  compared <- fy_app_compared(exposure, input, ctx)
  int_name <- paste0(fit_name, "_int")

  fy_app_say("  1. The interaction term\n\n")
  lost <- fy_app_lost_arg_lines(info$fit, fit_name)
  if (length(lost)) {
    fy_app_say_code(lost[nzchar(lost)], 7L)
  }
  if (length(added)) {
    fy_app_say("       ", int_name, " <- update(", fit_name, ", . ~ . + ",
        paste(added, collapse = " + "), ")\n\n", sep = "")
  } else {
    fy_app_say("       ", int_name, " <- ", fit_name,
        "   # your model carries it already\n\n", sep = "")
  }

  reference <- result$reference_cell
  if (is.null(reference)) {
    fy_app_say("  2. One estimate per subgroup, from that one model\n\n")
  } else {
    fy_app_say("  2. One estimate per combination of ", exposure, " and ", modifier,
        ", each\n     against the reference group, from that one model\n\n",
        sep = "")
  }
  fy_app_say_code(
    fy_app_lincom_code(info, int_name, exposure, compared, modifier = modifier,
                       levels = levels, reference_cell = reference,
                       exposure_levels = unique(stats::na.omit(
                         as.character(result$estimates$level)
                       ))),
    7L
  )

  fy_app_say("  3. The p-value for the interaction\n\n")
  columns <- tryCatch(
    fy_modifier_interaction_columns(info, exposure, modifier),
    error = function(e) character(0)
  )
  tests <- result$interaction_tests %||% list(result$interaction_test)
  taken <- vapply(tests, function(t) t$test %||% "", character(1))

  if (any(grepl("[Ll]ikelihood", taken))) {
    fy_app_say_code(fy_app_lrt_code(int_name, added, exposure, modifier), 7L)
  }
  if (any(grepl("Wald|F$", taken))) {
    fy_app_say_code(fy_app_joint_code(info, int_name, columns), 7L)
  }
  if (length(columns)) {
    fy_app_say("     Coefficients tested: ", paste(columns, collapse = ", "), "\n",
        sep = "")
  }
  for (test in tests) {
    if (is.null(test)) next
    fy_app_say("     Reported here:       ", test$test, " = ",
        fy_format_number(test$statistic), " on ", test$df, " df, p = ",
        fy_format_p(test$p.value), "\n", sep = "")
  }
  fy_app_say("\n")
}

# The code behind an estimate ------------------------------------------------

# What the Models tab writes out is meant to be run.
#
# It is not a description of what foresty does but the thing itself, named for
# this model and these variables: the same two rows of data, the same design
# matrix built the way the fitting function built its own, the same coefficient
# vector and covariance, the same error degrees of freedom and the same test.
# Pasting it into a script beside the model reproduces the number beside it
# rather than something close to it, which is what makes it worth reading.
#
# That is why it says `car::linearHypothesis.default()` and not
# `car::linearHypothesis()`: the method a fit dispatches to takes the model's
# own coefficients, covariance and degrees of freedom, which are not the ones
# the figure was drawn from where a robust variance was asked for. The
# hypothesis is a numeric matrix rather than a character formula for a related
# reason -- a coefficient may be named anything, and car's parser reads only
# some of those.
fy_app_say_code <- function(lines, indent) {
  pad <- strrep(" ", indent)
  fy_app_say(paste0(ifelse(nzchar(lines), paste0(pad, lines), ""), collapse = "\n"),
      "\n\n", sep = "")
  invisible(NULL)
}

# One estimate: the two rows, the design matrix, the contrast and the test.
# `modifier` wraps the last three of them in the function foresty runs once per
# subgroup, with the modifier held at that level in both rows.
#
# `reference_cell` is the other question the same machinery answers: the two
# rows then differ in both variables, one of them held at the combination every
# row is read against, so the loop runs over the combinations rather than over
# the subgroups.
fy_app_lincom_code <- function(info, fit_name, exposure, compared,
                               modifier = NULL, levels = NULL,
                               level_values = NULL, reference_cell = NULL,
                               exposure_levels = NULL) {
  design <- c(fy_app_design_lines(info, fit_name), "L <- X[2, ] - X[1, ]")
  test <- fy_app_test_lines(info, fit_name)
  values <- level_values %||% fy_app_level_values(info, modifier, levels)

  if (!is.null(modifier) && !is.null(reference_cell)) {
    return(c(
      fy_app_rows_lines(info, fit_name, exposure, compared, set = FALSE), "",
      fy_app_coef_lines(info, fit_name), "",
      "within <- function(level, exposure_level) {",
      paste0("  rows$", exposure, " <- c(",
             fy_app_level_values(info, exposure, reference_cell$exposure_level),
             ", exposure_level)   # the reference group, then this one"),
      paste0("  rows$", modifier, " <- c(",
             fy_app_level_values(info, modifier, reference_cell$modifier_level),
             ", level)"),
      fy_app_indent(c(design, "", test), 2L),
      "}",
      paste0("lapply(c(", values, "), function(level) {"),
      paste0("  lapply(c(",
             paste(fy_app_quote(exposure_levels), collapse = ", "),
             "), function(exposure_level) {"),
      "    within(level, exposure_level)",
      "  })",
      "})"
    ))
  }

  head <- c(fy_app_rows_lines(info, fit_name, exposure, compared), "",
            fy_app_coef_lines(info, fit_name), "")
  if (is.null(modifier)) {
    return(c(head, design, "", test))
  }
  c(head,
    "within <- function(level) {",
    paste0("  rows$", modifier, " <- level"),
    fy_app_indent(c(design, "", test), 2L),
    "}",
    paste0("lapply(c(", values, "), within)"))
}

# The levels of the modifier as the code has to write them: the numbers
# themselves for a flag coded 0 and 1, and quoted for a factor. A level put
# back into the data as a string of the wrong type is a column of the wrong
# type, and the design matrix built from it is not the model's.
fy_app_level_values <- function(info, modifier, levels) {
  x <- tryCatch(fy_variable(info, modifier), error = function(e) NULL)
  if (!is.null(x) && is.numeric(x)) {
    return(paste(levels, collapse = ", "))
  }
  if (!is.null(x) && is.logical(x)) {
    return(paste(toupper(as.character(as.logical(levels))), collapse = ", "))
  }
  paste(fy_app_quote(levels), collapse = ", ")
}

# The two rows the contrast is the difference between: one complete observation
# of the data, twice, with the exposure moved between them.
#
# The row is named as foresty named it. The source data is preferred over the
# model frame, since a model frame holds a splined variable as its basis matrix
# and setting the variable on such a row would change nothing.
#
# `set = FALSE` leaves the exposure where it is, for the loop that moves it and
# the modifier together and would otherwise be handed two rows already set.
fy_app_rows_lines <- function(info, fit_name, exposure, compared, set = TRUE) {
  where <- tryCatch(fy_reference_index(info), error = function(e) NULL)
  data_name <- fy_app_data_name(info$fit)
  taken <- if (is.null(where)) {
    paste0("model.frame(", fit_name, ")[c(1, 1), ]")
  } else if (where$from_data && !is.null(data_name)) {
    paste0(data_name, "[c(", where$index, ", ", where$index, "), ]")
  } else {
    paste0("model.frame(", fit_name, ")[c(", where$index, ", ", where$index,
           "), ]")
  }
  head <- paste0("rows <- ", taken, "   # one complete observation, twice")
  if (!isTRUE(set)) {
    return(head)
  }
  c(head,
    paste0("rows$", exposure, " <- c(", compared$from, ", ", compared$to, ")",
           compared$said))
}

# The name the model's own call knows its data by, so that the code names the
# data frame the reader has rather than a copy of it.
fy_app_data_name <- function(fit) {
  call <- tryCatch(stats::getCall(fit), error = function(e) NULL)
  if (is.null(call) || is.null(call$data) || !is.name(call$data)) {
    return(NULL)
  }
  as.character(call$data)
}

# The design matrix of those two rows, built as the fitting function builds its
# own.
#
# That means going back through model.frame() with the levels and the contrast
# coding the model was fitted under, so that the `predvars` of the terms object
# are used and a basis such as ns(no2, 3) or poly(maternal_age, 2) is reproduced
# with the knots and the centring it was fitted with rather than recomputed from
# two rows.
fy_app_design_lines <- function(info, fit_name) {
  aliased <- length(info$kept) < info$n_full
  out <- c(
    paste0("tt <- delete.response(terms(", fit_name, "))"),
    fy_app_frame_lines(info$fit, fit_name)
  )
  if (aliased) {
    out <- c(out, "X  <- X[, names(b), drop = FALSE]")
  }
  out
}

# Those two rows put back through model.frame() and model.matrix() with the
# levels and the contrast coding the model was fitted under.
#
# Most fits keep a copy of both, which is what `xlevels` and `contrasts` are.
# An S4 fit -- lme4's among them -- cannot be subsetted at all, so `$xlevels`
# on one is an error rather than a NULL, and the two are taken from its own
# model frame and design matrix instead. That is where the copies a fit keeps
# came from in the first place, so the two agree.
fy_app_frame_lines <- function(fit, name, indent = "") {
  if (!isS4(fit)) {
    return(paste0(indent, c(
      paste0("mf <- model.frame(tt, data = rows, xlev = ", name, "$xlevels,"),
      "                  na.action = na.pass)",
      paste0("X  <- model.matrix(tt, data = mf, contrasts.arg = ", name,
             "$contrasts)")
    )))
  }
  paste0(indent, c(
    "mf <- model.frame(tt, data = rows, na.action = na.pass,",
    paste0("                  xlev = .getXlevels(tt, model.frame(", name,
           ")))"),
    "X  <- model.matrix(tt, data = mf,",
    paste0("                   contrasts.arg = attr(model.matrix(", name,
           "), \"contrasts\"))")
  ))
}

# How the coefficients the contrast is taken over are asked for. Most fits
# report them with coef(); an lme4 fit keeps coef() for the coefficients of
# each group and reports the fixed effects, which are the ones this arithmetic
# is over, with fixef(). It is the rule fy_coefs() dispatches on, written out.
fy_app_coef_expr <- function(fit, name) {
  if (inherits(fit, "merMod")) {
    return(paste0("lme4::fixef(", name, ")"))
  }
  paste0("coef(", name, ")")
}

# The coefficients and the covariance the test is taken over. An aliased
# coefficient is NA and has no column in vcov(), so it is dropped from both and
# from the design matrix with them, which is what keeps the three lined up.
fy_app_coef_lines <- function(info, fit_name) {
  coefs <- fy_app_coef_expr(info$fit, fit_name)
  if (length(info$kept) < info$n_full) {
    return(c(
      paste0("b <- ", coefs),
      "b <- b[!is.na(b)]   # this model has aliased coefficients",
      paste0("V <- vcov(", fit_name, ")[names(b), names(b), drop = FALSE]")
    ))
  }
  c(paste0("b <- ", coefs),
    paste0("V <- vcov(", fit_name, ")",
           if (isTRUE(info$robust)) "   # already robust" else ""))
}

# The test itself, at the degrees of freedom foresty worked out: the residual
# degrees of freedom for a gaussian model, which is reported with t and F, and
# infinite for everything else, which is reported with the normal approximation
# and a Wald chi-square.
fy_app_test_lines <- function(info, fit_name, hypothesis = "L") {
  test <- if (is.finite(info$error_df)) "F" else "Chisq"
  df <- if (is.finite(info$error_df)) fy_trim_number(info$error_df) else "Inf"
  pad <- strrep(" ", nchar("car::linearHypothesis.default("))
  c(paste0("car::linearHypothesis.default(", fit_name, ", ", hypothesis,
           ", coef. = b, vcov. = V,"),
    paste0(pad, "error.df = ", df, ", test = \"", test, "\","),
    paste0(pad, "singular.ok = TRUE)"),
    "# The estimate is attr(., \"value\") of that and its standard error the",
    "# square root of attr(., \"vcov\"); the interval is formed from the two of",
    "# them at the confidence level you chose.")
}

# The joint test of the interaction coefficients: the rows of the identity
# matrix belonging to them, which is the hypothesis that every one of them is
# zero. Built by name rather than by position, since that is the only thing
# that survives an aliased coefficient.
fy_app_joint_code <- function(info, fit_name, columns) {
  c(fy_app_coef_lines(info, fit_name),
    paste0("columns <- c(", paste(fy_app_quote(columns), collapse = ", "), ")"),
    "L <- matrix(0, length(columns), length(b),",
    "            dimnames = list(columns, names(b)))",
    "L[cbind(seq_along(columns), match(columns, names(b)))] <- 1",
    "",
    fy_app_test_lines(info, fit_name)[1:3])
}

# The likelihood ratio test, as the arithmetic foresty does rather than as the
# anova() that comes to the same thing: twice the difference in log-likelihood,
# on the parameters the interaction spent.
fy_app_lrt_code <- function(fit_name, added, exposure, modifier) {
  terms <- paste(added %||% paste0(exposure, ":", modifier), collapse = " - ")
  c(paste0("reduced <- update(", fit_name, ", . ~ . - ", terms, ")"),
    "",
    paste0("statistic <- 2 * (as.numeric(logLik(", fit_name,
           ")) - as.numeric(logLik(reduced)))"),
    paste0("df <- attr(logLik(", fit_name,
           "), \"df\") - attr(logLik(reduced), \"df\")"),
    "pchisq(statistic, df = df, lower.tail = FALSE)",
    "",
    paste0("# anova(reduced, ", fit_name,
           ", test = \"LRT\") is the same test, said"),
    "# in one line. A REML-fitted lme4 model is refitted with maximum",
    "# likelihood first, since two REML likelihoods of different fixed",
    "# effects are not comparable.")
}

# The two values of the exposure the estimates are the difference between, for
# the code the Models tab writes out. They are the package's own, taken from
# the same function the figure took them from.
fy_app_compared <- function(exposure, input, ctx) {
  args <- fy_app_contrast_args(input, exposure, ctx)
  values <- tryCatch(
    fy_exposure_values(
      ctx$info, exposure,
      contrast = args$contrast,
      at = if (is.null(args$at)) NULL else c(args$at[[2L]], args$at[[3L]])
    ),
    error = function(e) NULL
  )
  if (is.null(values)) {
    return(list(from = "from", to = "to", said = ""))
  }
  first <- values[[length(values)]]
  said <- first$contrast_label %||% NA_character_
  list(
    from = fy_app_value_text(first$from),
    to = fy_app_value_text(first$to),
    said = if (is.na(said)) "" else paste0("   # ", said)
  )
}

fy_app_value_text <- function(x) {
  if (is.factor(x) || is.character(x)) {
    fy_app_quote(as.character(x))
  } else {
    fy_trim_number(round(as.numeric(x), 4))
  }
}

fy_app_term_labels <- function(fit) {
  out <- tryCatch(attr(stats::terms(fit), "term.labels"),
                  error = function(e) character(0))
  if (is.null(out)) character(0) else out
}

fy_app_formula_text <- function(fit) {
  out <- tryCatch(paste(deparse(stats::formula(fit)), collapse = " "),
                  error = function(e) "not available")
  gsub("\\s+", " ", out)
}

# Writing files --------------------------------------------------------------

# The figures on the screen, into a file.
#
# PNG through ragg where it is installed, since text at 300 dpi is what the
# journals ask for and what ragg draws is what the screen showed. SVG through
# svglite where it is installed and through the graphics device R was built
# with otherwise, so that the text stays text and the figure can be edited.
fy_app_save <- function(figures, file, format, input) {
  if (!length(figures)) {
    stop("there is no figure to save", call. = FALSE)
  }
  width <- fy_app_number(input$width, 10)
  height <- fy_app_number(input$height, 6) * length(figures)

  if (identical(format, "png")) {
    dpi <- fy_app_number(input$dpi, 300)
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(file, width = width, height = height, units = "in",
                    res = dpi, background = "white")
    } else {
      grDevices::png(file, width = width, height = height, units = "in",
                     res = dpi, bg = "white")
    }
  } else if (requireNamespace("svglite", quietly = TRUE)) {
    svglite::svglite(file, width = width, height = height, bg = "white")
  } else {
    if (!isTRUE(capabilities("cairo"))) {
      stop(
        "this build of R cannot write SVG. Install the svglite package with ",
        "install.packages(\"svglite\"), or download the figure as PNG.",
        call. = FALSE
      )
    }
    grDevices::svg(file, width = width, height = height, bg = "white")
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  # Built once the device is open, since how wide the plot in a figure has to
  # be is settled against the width it is being drawn at: a file downloaded at
  # the size on the screen has to be the figure that was on the screen. One
  # figure is printed as itself, which is what applies the floor; several are
  # each given it here, because stacking them hands patchwork the class and the
  # estimates of whichever it took them from.
  plot(if (length(figures) == 1L) {
    figures[[1L]]
  } else {
    patchwork::wrap_plots(
      lapply(figures, function(one) fy_app_plain(fy_floor_plot_width(one))),
      ncol = 1
    )
  })
  invisible(file)
}

# Several figures are several files, since figures that were not combined were
# not combined for a reason and stacking them into one image would put them
# back together. One file goes down as it is; several go down zipped.
fy_app_save_all <- function(figures, file, format, input) {
  if (length(figures) == 1L) {
    return(fy_app_save(figures, file, format, input))
  }
  fy_app_zip(fy_app_each(figures, format, function(one, path) {
    fy_app_save(one, path, format, input)
  }), file)
}

# The figure on a report page is the figure the buttons beside it write, so it
# is drawn at the size set there rather than at a size of the report's own: a
# page whose plot is squashed into three inches is not the figure that was
# being looked at.
fy_app_write_reports <- function(figures, file, input = list()) {
  width <- fy_app_number(input$width, 10)
  height <- fy_app_number(input$height, 6)

  if (length(figures) == 1L) {
    # foresty_report() names the file itself where it was handed one that is
    # not .html, and the browser is waiting for this one.
    page <- tempfile(fileext = ".html")
    foresty_report(figures[[1L]], file = page, width = width, height = height)
    file.copy(page, file, overwrite = TRUE)
    return(invisible(file))
  }
  fy_app_zip(fy_app_each(figures, "html", function(one, path) {
    foresty_report(one[[1L]], file = path, width = width, height = height)
  }), file)
}

# One file per figure, in a directory of their own, named for the pair each of
# them is of.
fy_app_each <- function(figures, ext, write) {
  dir <- tempfile("foresty-")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  names <- make.unique(vapply(seq_along(figures), function(i) {
    fy_file_slug(names(figures)[i] %||% paste0("figure_", i))
  }, character(1)), sep = "_")

  vapply(seq_along(figures), function(i) {
    path <- file.path(dir, paste0(names[i], ".", ext))
    write(figures[i], path)
    path
  }, character(1))
}

fy_app_zip <- function(paths, file) {
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = file, files = paths)
    return(invisible(file))
  }
  if (!nzchar(Sys.which("zip"))) {
    stop(
      "several figures come down as several files in a zip, which needs the ",
      "zip package: install.packages(\"zip\"). Or tick \"Combine the pairs ",
      "into one figure\", which is one figure and one file.",
      call. = FALSE
    )
  }
  # utils::zip() names its files relative to the working directory, so the
  # directory has to be the one the files are in for the length of the call.
  # It is read and the way back registered before it is changed, so that it is
  # put back however the call leaves -- including on an error inside it.
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(dirname(paths[1L]))
  utils::zip(zipfile = file, files = basename(paths), flags = "-q")
  invisible(file)
}
