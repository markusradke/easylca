#' Titanic passengers example data set
#'
#' The training data set from the \emph{titanic} R package, prepared for an analysis with \emph{easylca}.
#'
#' @format
#' A data frame with 891 rows and 9 columns:
#' \describe{
#'   \item{id}{Passenger ID}
#'   \item{survived}{Has the passenger survived the sinking of the ship?}
#'   \item{pasclass}{Passenger Class}
#'   \item{age}{Age of the passenger}
#'   \item{fare}{Ticket fare for the passenger}
#'   \item{nsibsp}{Number of siblings or spouses aboard, three levels:
#'    \describe{
#'     \item{1}{no siblings or spouses}
#'     \item{2}{1 sibling or spouse}
#'     \item{3}{2 or more siblings or spouses}
#'   }
#' }
#'   \item{nparchi}{Number of parents or children aboard, three levels:
#'    \describe{
#'     \item{1: }{no parents or children}
#'     \item{2: }{1 parent or children}
#'     \item{3: }{2 or more parents or children}
#'   }
#' }
#'  \item{isfem}{Is passenger female?}
#'   \item{port}{Port of embarkation, three levels:
#'    \describe{
#'     \item{1: }{Southampton}
#'     \item{2: }{Cherbourg}
#'     \item{3: }{Queenstown}
#'   }
#' }
#' }
#' @seealso run [generate_data_diagnosis_report()] to create an HTML-data report
#' @source <https://cran.r-project.org/web/packages/titanic/readme/README.html>
"titanic_passengers"

#' Titanic passengers example easylca settings
#'
#' A settings object with the correctly specified lca parameters for the [titanic_passengers] example data set.
#'
#' @format
#' An \emph{easylca} settings object with the following parameters:
#' \describe{
#'   \item{frame}{[titanic_passengers] data set}
#'   \item{analysis_name}{titanic}
#'   \item{id}{id}
#'   \item{nclasses}{10}
#'   \item{starts}{all start values set to 160}
#'   \item{cores}{use 16 cores for processing}
#'   \item{nominal}{\emph{port} and \emph{pasclass} are declared as nominal}
#'   \item{categorical}{\emph{survived, isfem, nsibsp}, and \emph{nparchi} declared as categorical (meaning: ordered)}
#'   \item{lmrlrt}{FALSE, no LMRLRT will be caluclated}
#'   \item{inflation, censoring}{no inflation or censoring for the continuous variables \emph{age} and \emph{fare}}
#' }
#' @seealso
#' \itemize{
#'  \item [titanic_passengers] for the corresponding  data set
#'  \item run [generate_data_diagnosis_report()] to create an HTML-data report
#' }
"titanic_settings"
