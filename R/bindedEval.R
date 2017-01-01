#' Evaluation with name binding
#'
#' Probides evaluation with name binding mimicking Haskell's `where` clause.
#' @docType package
#' @name bindedEval
#' 
#' @import utils
#' @importFrom igraph graph_from_adjacency_matrix topological.sort
#' @importFrom magrittr %>%
# requireNamespace("igraph")
# requireNamespace("magrittr")
swap_args <- function (f) {
  wrapper <- function (a, b) f(b, a)
  return(wrapper)
}

keep_list <- function (x, is_keeped) x[unlist(lapply(x, is_keeped))]

call_to_list <- function (parse_tree) {
  if(!is.call(parse_tree)) {
    return(list(parse_tree))
  } else {
    return(lapply(parse_tree, call_to_list))
  }
}

make_adjacency_matrix_from_bindings <- function(bindings) {
  n_bindings <- length(bindings)
  binded_names <- names(bindings)
  get_adjacent <- function (binding) {
    names_in_binding <- binding %>%
      call_to_list() %>%
      unlist(recursive = TRUE) %>%
      keep_list(is.name) %>%
      lapply(as.character) %>%
      unlist(recursive  = TRUE)
    return(binded_names %in% names_in_binding)
  }
  binding_adjacency_vector <- bindings %>%
  sapply(get_adjacent)
  binding_adjacency_matrix <- binding_adjacency_vector %>%
  matrix(nrow = n_bindings, ncol = n_bindings)
  return(binding_adjacency_matrix)
}

decide_order_to_eval <- function(bindings) {
  binded_names <- names(bindings)
  order_to_eval <- bindings %>%
    make_adjacency_matrix_from_bindings() %>%
    graph_from_adjacency_matrix() %>%
    topological.sort() %>%
    swap_args(`[`)(binded_names)
  return(order_to_eval)
}

#' @export
#' @name let
#' @title Evaluates expr with bindings. The binding in rhs are locked, so
#' @param ... binding
#' @param .in expression to be evaluated
#' @return the result of evaluation of `.in` with binding
#' @examples
#' velocity <- function (x1, x2, t1, t2) let(
#'   dx = x2 - x1,
#'   dt = t2 - t1,
#'   .in. = {
#'     dx / dt
#'   }
#' )
#' velocity(80, 90, 0, 0.5)
let <- function (..., .in) {
  bindings <- substitute(list(...)) %>% as.list() %>% tail(-1)
  binded_names <- names(bindings)
  # all delcaretions must be named
  stopifnot(all(nchar(binded_names) > 0))
  order_to_eval <- decide_order_to_eval(bindings)
  binding_env <- new.env()
  # to see variables in .in, parent.frame must be used
  parent.env(binding_env) <- parent.frame()
  for (binding in order_to_eval) {
    assign(binding, eval(bindings[[binding]], envir = binding_env), envir = binding_env)
    lockBinding(binding, binding_env)
  }
  # print(as.list(binding_env))
  eval(substitute(.in), envir = binding_env)
}

#' @export
#' @name where
#' @title Add binding to function.
#' @param FUN function
#' @param ... binding
#' @return FUN with binding
#' @examples
#' velocity <- (function (x1, x2, t1, t2)) %>%
#'   where(
#'     dx = x2 - x1,
#'     dt = t2 - t1)
#' velocity(80, 90, 0, 0.5)
where <- function (FUN, ...) {
  force(FUN)
  declaretions <- tail(substitute(list(...)), -1)
  declaretion_names <- names(declaretions)
  order_to_eval <- decide_order_to_eval(declaretions)
  names(order_to_eval) <- order_to_eval
  # all delcaretions must be named
  stopifnot(all(nchar(declaretion_names) > 0))
  f_with_declaretions <- function (...) {
    .orig <- list(...)
    call_as_org <- do.call(call, c("FUN", list(...)))
    matched_args <- match.call(FUN, call_as_org, envir = environment(FUN)) %>%
      as.list() %>%
      tail(-1)
    for (i in seq(length(matched_args))) {
      name <- names(matched_args)[[i]]
      assign(name, matched_args[[i]])
    }
    declared_env <- new.env()
    for (name in order_to_eval) {
      with(declared_env,
        assign(name, eval(declaretions[[name]])))
      lockBinding(name, declared_env)
    }
    parent.env(declared_env) <- environment(FUN)
    environment(FUN) <- declared_env
    return(FUN(...))
  }
  return(f_with_declaretions)
}
