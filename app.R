library(shiny)
library(visNetwork)
library(data.table)

# UI -------
ui <- fluidPage(
  titlePanel("🔍 Interactive confounder selection"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(align="center",
      div(style="display:inline-block; margin:3px", 
          actionButton("startover_btn", "New", icon=icon("file"))),
      div(style="display:inline-block; margin:3px", 
          downloadButton("save_btn", "Save")),
      div(style="display:inline-block; margin:3px", 
          actionButton("undo_btn", "Undo", icon=icon("rotate-left"))),
      div(style="display:inline-block; margin:3px", 
          actionButton("redo_btn", "Redo", icon=icon("rotate-right")))
      ),
      # hr(),
      # verbatimTextOutput("node.sel"), 
      # verbatimTextOutput("edge.sel"),
      # verbatimTextOutput("edge.hover"),
      hr(),
      fluidRow(align="center", tableOutput("out.edges")),
      fileInput("file_graph", "Load graph", accept=c(".rda", ".RData")),
      hr(),
      fluidRow(align="center", tableOutput("out.adj.sets")),
      hr(),
      p("ℹ See ", 
        a("Guo and Zhao (2023)", href="https://arxiv.org/abs/2309.06053"), 
        "for details of this method."),
      p("⧰ Report issues", 
        a("here.", href="https://github.com/richardkwo/InteractiveConfSel/issues")),
      p("© ", a("Richard Guo", href="https://unbiased.co.in")),
      width=5
    ),
    mainPanel(
      visNetworkOutput("network"),
      h4(textOutput("out.mincut"), 
         actionLink("record_link", ""), align="right", style="color:orange"),
      span(textOutput("out.tip"), align="right", style="color:grey"),
      width=7
    )
  )
)

# Server -------
server <- function(input, output) {
  # initialize --------
  # .nodes.init <- data.table(id = c(1, 2, 3, 4, 5),
  #                           shadow = c(TRUE, TRUE, FALSE, FALSE, FALSE),
  #                           level = c(1,1,2,2,3),
  #                           label = c("X", "Y", "T", "F", "R"))
  # .edges.init <- data.table(from=c(3, 4, 3, 5, 5, 5, 5),
  #                           to=c(4, 2, 1, 1, 2, 3, 4),
  #                           id=c(1,2,3,4, 5, 6, 7),
  #                           expandable=TRUE)
  # 
  # .edges.init$expandable[c(1,3)] <- FALSE

  .nodes.init <- data.table(id = c(1, 2),
                            shadow = c(TRUE, TRUE),
                            level = c(1,1),
                            label = c("X", "Y"))
  .edges.init <- data.table(from=c(1),
                            to=c(2),
                            id=c(1),
                            expandable=TRUE)

  setkey(.nodes.init, id)
  setkey(.edges.init, id)
  
  graph <- reactiveValues(nodes=.nodes.init, edges=.edges.init)
  
  suff.adj.sets <- reactiveVal(list())    # record the adj. sets found
  
  primary.adj.set <- reactiveVal()
  v.first <- reactiveVal(TRUE)            # for `enterMediatorsModal'
  
  # map selected node
  node.selected <- reactive({
    if (is.null(input$network_selected) || input$network_selected=="") {
      NULL
    } else {
      as.integer(input$network_selected)
    }
  })
  
  # for undo
  graphs.undo.saved <- reactiveVal(list())
  graphs.redo.saved <- reactiveVal(list())
  
  # get label 
  
  get.v.label <- function(.v) {
    if (!is.null(.v)) {
      graph$nodes[.v]$label
    }
  }
  
  get.v.from <- function(.e, label=FALSE) {
    if (!is.null(.e)) {
      if (label) {
        get.v.label(graph$edges[.e]$from)
      } else {
        graph$edges[.e]$from
      }
    }
  }
  
  get.v.to <- function(.e, label=FALSE) {
    if (!is.null(.e)) {
      if (label) {
        get.v.label(graph$edges[.e]$to)
      } else {
        graph$edges[.e]$to
      }
    }
  }
  
  get.S <- function(label=FALSE) {
    .S <- setdiff(graph$nodes$id, c(1,2))
    if (label) {
      get.v.label(.S)
    } else {
      .S
    }
  }
  
  get.V <- function(label=FALSE) {
    V <- 1:nrow(graph$nodes)
    if (label) {
      return(get.v.label(V))
    } else {
      return(V)
    }
  }
  
  expand.graph <- function(graph, edge, primary.set) {
    primary.set <- setdiff(primary.set, get.V(label=TRUE))
    # empty set 
    if (length(primary.set)==0) {
      edges.new <- graph$edges[-edge]
      edges.new$id <- 1:nrow(edges.new)
      setkey(edges.new, id)
      list(nodes=graph$nodes, 
           edges=edges.new)
    } else {
      v.id.old <- graph$nodes$id
      v.id.new <- max(graph$nodes$id) + (1:length(primary.set))
      level.new <- max(graph$nodes$level[get.v.from(edge)], graph$nodes$level[get.v.to(edge)]) + 1
      nodes.new <- rbind(graph$nodes, data.table(id = v.id.new, 
                              shadow = FALSE,
                              level = level.new,
                              label = primary.set))
      setkey(nodes.new, id)
      edges.add <- data.table(expand.grid(from=v.id.new, to=c(v.id.old, v.id.new)))
      edges.add <- edges.add[from > to]
      edges.add$id <- max(graph$edges$id) + (1:nrow(edges.add))
      edges.add$expandable <- TRUE
      edges.new <- rbind(graph$edges[-edge], edges.add)
      edges.new$id <- 1:nrow(edges.new)
      setkey(edges.new, id)
      list(nodes=nodes.new, edges=edges.new)
    }
  }
  
  # render -------
  output$network <- renderVisNetwork({
    edges.render <- graph$edges
    # edge specific color
    edges.render$color.color <- "grey"
    edges.render$color.highlight <- "red"
    edges.render$color.hover <- "grey"
    edges.render$color.color[mincut.info()$edges] <- "orange"
    edges.render$color.hover[mincut.info()$edges] <- "orange"
    edges.render[expandable==FALSE]$color.color <- "black"
    edges.render[expandable==FALSE]$color.hover <- "black"
    edges.render[expandable==FALSE]$color.highlight <- "black"
    # edge specific dash
    edges.render$dashes <- TRUE
    edges.render[expandable==FALSE]$dashes <- FALSE
    # render
    visNetwork(graph$nodes, edges.render) %>% 
      visEdges(arrows="from;to", color=list(highlight="red"),
               smooth=list(enabled=TRUE)) %>% 
      visHierarchicalLayout(direction="DU", blockShifting=TRUE, levelSeparation=80) %>%
      visInteraction(zoomView=FALSE, selectConnectedEdges = FALSE, 
                     hover = TRUE) %>% 
      visOptions(nodesIdSelection=TRUE) %>%
      visEvents(click = "function(properties) {
        Shiny.onInputChange('edge.sel', properties.edges);}", 
                hoverEdge = "function(edges) {
        Shiny.onInputChange('edge.hover', edges.edge);}", 
                doubleClick = "function(click) {
        Shiny.onInputChange('double_click', click);}")
  })
  
  # download graph ------
  output$save_btn <- downloadHandler("graph.RData", 
    content=function(file) {
      nodes <- data.frame(graph$nodes)
      edges <- data.frame(graph$edges)
      suff.adj.sets <- suff.adj.sets()
      save(nodes,
           edges,
           suff.adj.sets,
           file=file)
    }
  )
  
  # upload graph ------
  observeEvent(input$file_graph, {
    if (!is.null(input$file_graph)) {
      .env <- new.env()
      load(input$file_graph$datapath, .env)
      .env$nodes <- data.table(.env$nodes)
      .env$edges <- data.table(.env$edges)
      setkey(.env$nodes, id)
      setkey(.env$edges, id)
      graph$nodes <- .env$nodes
      graph$edges <- .env$edges
      save.undo(reset=TRUE)
      suff.adj.sets(.env$suff.adj.sets)
    }
  })
  
  # info panel -----
  output$out.edges <- renderTable({
    print(graph$nodes)
    .df <- graph$edges[,1:2]
    .df$from <- graph$nodes$label[.df$from]
    .df$to <- graph$nodes$label[.df$to]
    .df$label <- ""
    .df$label[mincut.info()$edges] <- "on min-cut"
    .df$expand <- ""
    .df$expand[!graph$edges$expandable] <- "cannot expand"
    .df
  }, digits=0, spacing="s", colnames = FALSE, align="c", rownames = TRUE)
  
  output$node.sel <- renderText({
    paste("node.sel", input$network_selected, sep=": ")
  })
  
  output$out.tip <- renderText({
    if (!is.null(input$edge.sel) && input$edge.sel<=nrow(graph$edges) && 
        is.finite(mincut.info()$value) && mincut.info()$value > 0) {
      if (graph$edges[input$edge.sel]$expandable) {
        sprintf("Double click to examine %s <-> %s", 
                get.v.from(input$edge.sel, label = TRUE),
                get.v.to(input$edge.sel, label = TRUE))
      } else {
        sprintf("%s <-> %s cannot be expanded", 
                get.v.from(input$edge.sel, label = TRUE),
                get.v.to(input$edge.sel, label = TRUE))
      }
    } else if (!is.null(node.selected())) {
      sprintf("Double click to rename %s",
              get.v.label(node.selected()))
    }
  })
  output$edge.hover <- renderText({
    paste("edge.hover", input$edge.hover, sep=": ")
  })
  
  # undo and redo ------
  # backup for undo
  save.undo <- function(reset=FALSE) {
    if (reset) {
      graphs.undo.saved(list())
      graphs.redo.saved(list())
    } else {
      graphs.undo.saved(c(list(list(nodes=graph$nodes, edges=graph$edges)), 
                     graphs.undo.saved()))
    }
  }
  
  observeEvent(input$undo_btn, {
    if (length(graphs.undo.saved()) > 0) {
      # move this to redo
      graphs.redo.saved(c(list(list(nodes=graph$nodes, edges=graph$edges)), 
                          graphs.redo.saved()))
      # revert
      graph$nodes <- graphs.undo.saved()[[1]]$nodes
      graph$edges <- graphs.undo.saved()[[1]]$edges
      # pop undo
      graphs.undo.saved(graphs.undo.saved()[-1])
    }
  })
  
  observeEvent(input$redo_btn, {
    if (length(graphs.redo.saved()) > 0) {
      # save undo
      save.undo()
      # revert
      graph$nodes <- graphs.redo.saved()[[1]]$nodes
      graph$edges <- graphs.redo.saved()[[1]]$edges
      # pop redo
      graphs.redo.saved(graphs.redo.saved()[-1])
    }
  })
  
  # min-cut -----
  make.igraph <- function(nodes, edges) {
    edges$capacity <- 1
    edges$capacity[!edges$expandable] <- 2e5
    edges <- edges[, c("from", "to", "capacity")]
    nodes <- nodes[, c("id", "label")]
    igraph::graph_from_data_frame(edges, directed=FALSE, vertices=nodes)
  }
  
  mincut.info <- reactive({
    g <- make.igraph(graph$nodes, graph$edges)
    .value <- igraph::min_cut(g, 1, 2)
    if (.value > 1e5) {
      # no cut exists
      return(list(value=Inf, edges=NULL))
    } else if (.value == 0) {
      # already disconnected
      return(list(value=0, edges=NULL))
    }
    # find edges that are on the min-cut
    exp.edges <- which(graph$edges$expandable)
    .value.rm.edge <- sapply(exp.edges, function(.e) {
      .g <- igraph::delete.edges(g, .e)
      igraph::min_cut(.g, 1, 2)
    })
    return(list(value=.value, 
                edges=exp.edges[.value.rm.edge == .value - 1]))
  })
  
  output$out.mincut <- renderText({
    if (is.infinite(mincut.info()$value)) {
      sprintf("No sufficient adjustment set can be found")
    } else if (mincut.info()$value == 0){
      # found a sufficient adjustment set
      sprintf("Sufficient adjustment set found: {%s}", 
              paste(get.S(label=TRUE), collapse=", "))
    } else if (mincut.info()$value == 1) {
      sprintf("min-cut = 1 edge")
    } else {
      sprintf("min-cut = %s edges", mincut.info()$value)
    }
  })
  
  observeEvent(mincut.info(), {
    if (mincut.info()$value == 0) {
      updateActionLink(inputId = "record_link", 
                       label="Record this set")
    } else {
      updateActionLink(inputId = "record_link", label="", icon=NULL)
    }
  })
  
  # record set found -----
  output$out.adj.sets <- renderTable({
    if (length(suff.adj.sets()) > 0) {
      .df <- data.frame(S=sapply(suff.adj.sets(), paste, collapse=", "))
      colnames(.df)[1] <- "Sufficient adjustment set"
      .df
    }
  }, align="c")
  
  observeEvent(input$record_link, {
    suff.adj.sets(c(list(get.S(label=TRUE)), suff.adj.sets()))
    updateActionLink(inputId = "record_link", label="", icon=NULL)
  })
  
  # rename node -------
  renameNodeModal <- function(node, removed=NULL) {
    modalDialog(
      selectizeInput("rename.input", 
                     sprintf("Enter the new name of %s:", get.v.label(node)), 
                     choices=NULL,
                     selected=NULL,
                     multiple=FALSE, 
                     options=list('plugins' = list('remove_button'),
                                  'create' = TRUE,
                                  'persist' = TRUE)),
      if (!is.null(removed)) {
        div(tags$b(sprintf("%s already exists and is removed.",
                           paste(removed, collapse = ","))), 
            style = "color: red;")
      },
      footer = tagList(
        actionButton("rename_ok_btn", "OK"),
        modalButton("Cancel"))
    )
  }
  
  observeEvent(input$rename_ok_btn, {
    if (input$rename.input %in% get.V(label=TRUE)) {
      showModal(renameNodeModal(node.selected(), removed=input$rename.input))
    } else if (input$rename.input!="") {
      removeModal()
      # backup for undo
      save.undo()
      # rename
      graph$nodes[node.selected()]$label <- input$rename.input
    }
  })
  
  # expansion ------
  expandChoiceModal <- function(edge) {
    modalDialog(
      h4(sprintf("🤔 Is there a primary adjustment set for %s and %s?", 
                   get.v.from(edge, label=TRUE), get.v.to(edge, label=TRUE))),
      easyClose = TRUE,
      size="m",
      footer = tagList(
        actionButton("expand_dialog_keyin_btn", "Yes", icon=icon("check")),
        actionButton("expand_dialog_wizard_btn", "Maybe, help me find one", icon=icon("magnifying-glass")),
        actionButton("expand_dialog_no_btn", "No, there isn't one", icon=icon("xmark")),
        modalButton("Cancel")
      )
    )}
  
  observeEvent(input$startover_btn, {
    graph$nodes <- .nodes.init
    graph$edges <- .edges.init
    save.undo(reset=TRUE)
  })
  
  observeEvent(input$double_click, {
    if (!is.null(input$edge.sel) && is.finite(mincut.info()$value) && 
        mincut.info()$value > 0 && graph$edges$expandable[input$edge.sel]) {
      showModal(expandChoiceModal(input$edge.sel))
    } else if (!is.null(node.selected())) {
      showModal(renameNodeModal(node.selected()))
    }
  })
  
  # mark not expandable -----
  mark.not.expandable <- function(edge) {
    # backup for undo
    save.undo()
    # update
    graph$edges$expandable[edge] <- FALSE
  }
  
  observeEvent(input$expand_dialog_no_btn, {
    removeModal()
    mark.not.expandable(input$edge.sel)
  })
  
  # expansion by entering primary -----
  enterPrimaryAdjModal <- function(edge, selected=NULL, removed=NULL) {
    .u <- get.v.from(edge)
    .v <- get.v.to(edge)
    .S <- setdiff(get.S(), c(.u, .v))
    .prompt <- sprintf("Enter a set of observed variables that control for %s <-> %s:", 
                         get.v.label(.u), get.v.label(.v))
    modalDialog(
      selectizeInput("primary.input", 
                     .prompt, 
                  choices=union(get.v.label(.S), selected),
                  selected=selected,
                  multiple=TRUE, 
                  options=list('plugins' = list('remove_button'),
                                 'create' = TRUE,
                                 'persist' = TRUE)),
      p("(Leave it blank if none is needed.)"),
      if (!is.null(removed)) {
        div(tags$b(sprintf(ifelse(length(removed)==1,
                                  "%s is removed.",
                                  "%s are removed."),
                           paste(removed, collapse = ",")), style = "color: red;"))
      },
      footer = tagList(
        actionButton("primary_adj_enter_ok_btn", "OK"),
        modalButton("Cancel"))
    )
  }
  
  observeEvent(input$expand_dialog_keyin_btn, {
    removeModal()
    showModal(enterPrimaryAdjModal(input$edge.sel))
  })
  
  observeEvent(input$primary_adj_enter_ok_btn, {
    .u <- get.v.from(input$edge.sel)
    .v <- get.v.to(input$edge.sel)
    removed <- intersect(input$primary.input, 
                         get.v.label(c(1,2,.u,.v)))
    print(removed)
    if (length(removed) > 0) {
      showModal(enterPrimaryAdjModal(input$edge.sel, 
                                     selected=setdiff(input$primary.input, removed), 
                                     removed=removed))
    } else {
      # input is legit
      removeModal()
      # save undo
      save.undo()
      # expand
      .new.graph <- expand.graph(graph, input$edge.sel, input$primary.input)
      graph$nodes <- .new.graph$nodes
      graph$edges <- .new.graph$edges
    }
  })
  
  # find primary wizard --------
  get.relative.set <- function(edge) {
    .u <- get.v.from(edge)
    .v <- get.v.to(edge)
    .rel <- union(primary.adj.set(), get.v.label(setdiff(get.S(), c(.u, .v))))
    if (length(.rel) == 0) {
      return(NULL)
    } else {
      return(.rel)
    }
  }
  
  primaryWizardModal <- function(edge) {
    .u <- get.v.from(edge)
    .v <- get.v.to(edge)
    rel.set <- get.relative.set(edge)
    .title <- sprintf("Finding a primary adjustment set for %s <-> %s", 
            get.v.label(.u), get.v.label(.v))
    .display <- sprintf("ℹ Current candidate: {%s}", 
                        paste(primary.adj.set(), collapse = ", "))
    .footer <- tagList(
      actionButton("primary_wizard_yes_btn", "Yes", icon=icon("check")),
      actionButton("primary_wizard_no_btn", "No", icon=icon("xmark")), 
      modalButton("Cancel"))
    if (length(rel.set)==0) modalDialog(
      p(.display),
      h4(sprintf("🤔 Is there a common cause of %s and %s?", 
                 get.v.label(.u), get.v.label(.v))),
      p(sprintf("That is, any variable v such that %s <-- v --> %s",
                get.v.label(.u), get.v.label(.v))),
      title=.title,
      footer=.footer) else modalDialog(
          p(.display),
          h4(sprintf("🤔 Is there a common cause of %s and %s that is not blocked by %s?", 
                     get.v.label(.u), get.v.label(.v), 
                     paste(rel.set, collapse = ", "))),
          p("That is, any variable v such that"),
          tags$ul(
            tags$li(sprintf("v --> %s is not block by {%s},", 
                            get.v.label(.u), paste(c(rel.set, get.v.label(.v)), collapse = ", "))),
            tags$li(sprintf("v --> %s is not block by {%s}.", 
                            get.v.label(.v), paste(c(rel.set, get.v.label(.u)), collapse = ", ")))),
          title=.title,
          footer=.footer)
  }
  
  # start the wizard
  observeEvent(input$expand_dialog_wizard_btn, {
    if (!is.null(input$edge.sel) && is.finite(mincut.info()$value) && 
        mincut.info()$value > 0 && graph$edges$expandable[input$edge.sel]) {
      primary.adj.set(NULL)
      showModal(primaryWizardModal(input$edge.sel))
    }
  })
  
  # enter a common ancestor ------
  enterCommonAncestorModal <- function(edge, removed=NULL) {
    .u <- get.v.from(edge)
    .v <- get.v.to(edge)
    rel.set <- get.relative.set(edge)
    if (length(rel.set)==0) {
      .prompt <- sprintf("Enter a common cause of %s and %s:", 
                         get.v.label(.u), get.v.label(.v))
    } else {
      .prompt <- sprintf("Enter a common cause of %s and %s that is not blocked by %s:", 
                         get.v.label(.u), get.v.label(.v), 
                         paste(rel.set, collapse = ", "))
    }
    modalDialog(
      selectizeInput("common.ancestor.input", 
                     .prompt, 
                     choices=NULL,
                     multiple=FALSE, 
                     options=list('plugins' = list('remove_button'),
                                  'create' = TRUE,
                                  'persist' = TRUE)),
      if (!is.null(removed)) {
        div(tags$b(sprintf("%s already exists and is removed.", removed), 
                   style = "color: red;"))
      },
      footer = tagList(
        actionButton("common_ancestor_enter_ok_btn", "OK"),
        modalButton("Cancel"))
    )
  }
  
  observeEvent(input$primary_wizard_yes_btn, {
    showModal(enterCommonAncestorModal(input$edge.sel))
  })
  
  observeEvent(input$common_ancestor_enter_ok_btn, {
    v.existing <- c(primary.adj.set(), get.V(label=TRUE))
    if (input$common.ancestor.input=="") {
      showModal(enterCommonAncestorModal(input$edge.sel))
    } else if (input$common.ancestor.input %in% v.existing) {
      showModal(enterCommonAncestorModal(input$edge.sel, removed=input$common.ancestor.input))
    } else {
      removeModal()
      # check if the common ancestor can be controlled for
      showModal(checkCommonAncestorObsModal(input$common.ancestor.input))
    }
  })
  
  # check if common ancestor is observed -----
  checkCommonAncestorObsModal <- function(common.ancestor) {
    modalDialog(
      h4(sprintf("🤔 Is %s observed?", common.ancestor)),
      footer = tagList(
        actionButton("common_ancestor_obs_yes", "Yes", icon=icon("check")),
        actionButton("common_ancestor_obs_no", "No", icon=icon("xmark")))
    )
  }
  
  # if observed, add to primary set and resume wizard
  observeEvent(input$common_ancestor_obs_yes, {
    primary.adj.set(union(primary.adj.set(), input$common.ancestor.input))
    removeModal()
    showModal(primaryWizardModal(input$edge.sel))
  })
  
  # query mediators to control an observed common ancestor -----
  enterMediatorsModal <- function(common.ancestor, edge, 
                                  selected=NULL, removed=NULL) {
    if (v.first()) {
      v <- get.v.from(edge)
    } else {
      v <- get.v.to(edge)
    }
    if (v.first()) {
      .prompt <- sprintf("🤔 Is there a set of observed mediators that can block %s <-- %s?", 
                         get.v.label(v), common.ancestor)
    } else {
      .prompt <- sprintf("🤔 Is there a set of observed mediators that can block %s --> %s?", 
                         common.ancestor, get.v.label(v))
    }
    
    modalDialog(
      selectizeInput("mediators.input", 
                     .prompt, 
                     choices=union(selected, get.relative.set(edge)),
                     selected=selected,
                     multiple=TRUE, 
                     options=list('plugins' = list('remove_button'),
                                  'create' = TRUE,
                                  'persist' = TRUE)),
      if (!is.null(removed)) {
        div(tags$b(sprintf(ifelse(length(removed)==1, 
                                  "%s is removed.",
                                  "%s are removed."),
                           paste(removed, collapse = ",")), style = "color: red;"))
      },
      title=sprintf("Controlling %s <-- %s --> %s by mediators", 
                    get.v.from(edge, label=TRUE), 
                    common.ancestor, 
                    get.v.to(edge, label=TRUE)),
      footer = tagList(
        actionButton("mediator_enter_yes_btn", "Yes, these are the mediators",
                     icon=icon("check")),
        actionButton("mediator_enter_no_btn", "No, I don't have such mediators",
                     icon=icon("xmark")), 
        modalButton("Cancel"))
    )
  }
  
  # if common ancestor is not observed, try to find mediators
  observeEvent(input$common_ancestor_obs_no, {
    common.ansestor <- input$common.ancestor.input
    removeModal()
    v.first(TRUE)
    showModal(enterMediatorsModal(common.ansestor, input$edge.sel))
  })
  
  # mediators entered
  observeEvent(input$mediator_enter_yes_btn, {
    # check input
    v.forbidden <- c(get.v.from(input$edge.sel, label=TRUE),
                     get.v.to(input$edge.sel, label=TRUE),
                     get.v.label(c(1,2)), input$common.ancestor.input)
    removed <- intersect(input$mediators.input, v.forbidden)
    if (!is.null(removed) & length(removed)>0) {
      showModal(enterMediatorsModal(input$common.ancestor.input, 
                                    input$edge.sel,
                                    selected=setdiff(input$mediators.input, removed), 
                                    removed=removed))}
    else if (length(input$mediators.input) > 0) {
      # found mediators: add mediators to candidate primary and resume wizard
      primary.adj.set(union(primary.adj.set(), input$mediators.input))
      cat("current prim:")
      print(primary.adj.set())
      removeModal()
      showModal(primaryWizardModal(input$edge.sel))
    }
  })
  
  # mediators cannot be found 
  observeEvent(input$mediator_enter_no_btn, {
    # if failed on the first vertex, then query mediator for the second vertex
    if (v.first()) {
      v.first(FALSE)
      showModal(enterMediatorsModal(input$common.ancestor.input, input$edge.sel))
    } else {
      # must failed on both vertices: this common ancestor cannot be controlled
      showModal(primaryResultModal(input$edge.sel, found=FALSE))
    }
  })
  
  # result of primary set wizard ------
  primaryResultModal <- function(edge, found=FALSE) {
    message("result: ", primary.adj.set())
    .u <- get.v.from(edge)
    .v <- get.v.to(edge)
    rel.set <- setdiff(get.S(), c(.u, .v))
    new.V <- setdiff(primary.adj.set(), get.V(label=TRUE))
    if (found) {
      # found a primary set
      if (length(primary.adj.set())==0) {
        .title <- sprintf("😃 The empty set is a primary adjustment set for %s and %s.", 
                          get.v.label(.u), get.v.label(.v))
      } else {
        .title <- sprintf("😃 {%s} is a primary adjustment set for %s and %s.", 
                          paste(primary.adj.set(), collapse = ", "),
                          get.v.label(.u), get.v.label(.v))
      }
      modalDialog(
        h4(sprintf("Expand %s <-> %s using this set?", 
                   get.v.label(.u), get.v.label(.v))),
        if (length(new.V) > 0) {
          p("If so, ", strong(sprintf("%s", paste(new.V, collapse=", "))), 
                              " will be added to the graph.")
        },
        title=.title,
        footer = tagList(
          actionButton("primary_wizard_expand_yes_btn", "Yes", 
                       icon=icon("check")),
          actionButton("primary_wizard_expand_maybe_btn", "Maybe, let me think about it", 
                       icon=icon("magnifying-glass")),
          modalButton("No", icon=icon("xmark"))))
    } else {
      # cannot find a primary set
      .title <- sprintf("🙁 Cannot find a primary adjustment set for %s and %s.", 
                        get.v.label(.u), get.v.label(.v))
      modalDialog(
        h4(sprintf("Mark %s <-> %s as not expandable?", 
                   get.v.label(.u), get.v.label(.v))),
        title=.title,
        footer = tagList(
          actionButton("primary_wizard_mark_yes_btn", "Yes", icon=icon("check")),
          modalButton("No", icon=icon("xmark"))))
    }
  }
  
  # no more common ancestor: found a primary set
  observeEvent(input$primary_wizard_no_btn, {
    showModal(primaryResultModal(input$edge.sel, found=TRUE))
  })
  
  # expand the edge with the primary set found
  observeEvent(input$primary_wizard_expand_yes_btn, {
    removeModal()
    # backup for undo
    save.undo()
    # expand
    .new.graph <- expand.graph(graph, input$edge.sel, primary.adj.set())
    graph$nodes <- .new.graph$nodes
    graph$edges <- .new.graph$edges
    primary.adj.set(NULL)
  })
  
  # use the primary set found as an input to expand the edge
  observeEvent(input$primary_wizard_expand_maybe_btn, {
    removeModal()
    .primary <- primary.adj.set()
    primary.adj.set(NULL)
    showModal(enterPrimaryAdjModal(input$edge.sel, 
                                   selected=.primary))
  })
  
  # not found: mark edge as not expandable
  observeEvent(input$primary_wizard_mark_yes_btn, {
    removeModal()
    mark.not.expandable(input$edge.sel)
  })
  
}

shinyApp(ui = ui, server = server)