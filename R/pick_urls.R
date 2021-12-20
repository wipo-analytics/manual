#' Extract URLs and email addresses from text
#' 
#' @description This code was written by Mikko Korpela at
#'   https://github.com/mvkorpel/pickURL. Please use the Mikko Korpela's
#'   repository if you wish to use this code.
#'   
#' \code{pick_urls} extracts individual \acronym{URL}s and email
#' addresses from the input text. The function recognizes comma and
#' whitespace as characters separating individual \acronym{URL}s, as
#' is the specification of the \acronym{URL} field in the DESCRIPTION
#' file of \R packages (R Core Team, 2017). See \sQuote{Commas and
#' Whitespace} for details.
#'
#' @section Details:
#'
#' Compatibility with the Internationalized Resource Identifier (IRI)
#' specification (Duerst and Suignard, 2005) has not been assessed
#' carefully. However, the function will accept non-ASCII characters
#' (as opposed to splitting the string).
#'
#' Invalid UTF-8 strings are handled by keeping the valid (ASCII)
#' bytes and discarding the rest. This means that URLs can still be
#' picked up from a \code{"latin1"} encoded string falsely marked as
#' having a \code{"UTF-8"} \code{\link{Encoding}} or when such a
#' string has an \code{"unknown"} encoding in a UTF-8 locale.
#'
#' The function can remove delimiting brackets, some other punctuation
#' or simple LaTeX markup around \acronym{URL}s.
#'
#' Note that the function looks for matching delimiting brackets
#' across input string boundaries when \code{single_item} is
#' \code{FALSE}. Also \code{rm_endpunct} works across
#' strings. Therefore it may be best that strings originating from
#' different input files or otherwise non-consecutive input lines are
#' processed separately, with multiple calls to this function. See
#' \code{collapse_x}.
#'
#' The default \code{url_pattern} means that the \acronym{URI} scheme
#' must be http, https or ftp and that the scheme must be followed by
#' \code{"://"} and at least one character, indicating the presence of
#' an authority component. See \code{\link{regex}}. For example,
#' setting \code{url_pattern} to \code{"^mailto:."} would allow email
#' \acronym{URL}s to be returned.
#'
#' The default \code{email_pattern} requires that the domain portion
#' after \code{"@@"} has at least two parts separated by a
#' \code{"."}. This rules out addresses such as
#' \code{"root@@localhost"} and avoids some false positives. Also
#' email addresses with a literal IP domain are dropped when the
#' domain is in square brackets.
#'
#' When \code{single_item} is \code{TRUE}, no more than one item is
#' extracted from each input string. If also \code{collapse_x} is
#' \code{TRUE}, then no more than one item is extracted from each
#' group of concatenated input items. The function looks for a
#' \acronym{URL} scheme and a following \code{":"}. In case of no
#' match, the first substring looking like an email address is
#' selected if \code{plain_email} is \code{TRUE}. A \acronym{URL}
#' between double quotes (\code{"\""}) or angle brackets (\code{"<"}
#' and \code{">"}) takes precedence over a \acronym{URL} without such
#' delimiters. A \acronym{URL} may be rejected by the filtering stage
#' (see \code{url_pattern}), in which case it does not matter if an
#' email address was also found: no results are returned for the input
#' string in question.
#'
#' \subsection{Commas and Whitespace}{
#'
#' The comma is a problematic \acronym{URL} separator, because it is a
#' valid character in some parts of a \acronym{URL} (Berners-Lee,
#' Fielding, and Masinter, 2005). The function estimates which commas
#' should remain as part of a \acronym{URL}. Misclassifications are
#' possible.
#'
#' Text between a pair of double quotes or angle brackets
#' (\acronym{URL} scheme required after the opening delimiter) is
#' mostly interpreted as representing a single \acronym{URL}, but
#' commas are still checked, and the \acronym{URL} is cut when
#' necessary. It is possible to use multiple lines (but not multiple
#' strings unless \code{collapse_x} is \code{TRUE}) for a long
#' \acronym{URL} when delimited by \code{"<"} and \code{">"}:
#' whitespace is removed when it occurs after the \code{":"} that
#' follows the \acronym{URL} scheme.
#'
#' Whitespace, i.e. tabs and spaces, and commas are allowed in plain
#' email addresses: in a double quoted local part, or in a domain
#' literal delimited by square brackets (Resnick, 2008). These are
#' accepted by the function.
#'
#' }
#'
#' @param x a \code{character} vector containing the input text.
#'
#' @param plain_email a \code{logical} flag. If \code{TRUE}, the
#'   function also looks for plain email addresses (i.e. not formatted
#'   as a mailto \acronym{URL}). The default is to use the same
#'   value as for \code{all_email} (\code{FALSE}).
#'
#' @param single_item a \code{logical} flag. If \code{TRUE}, the
#'   function looks for a single \acronym{URL} or email address per
#'   string instead of splitting each string into multiple potential
#'   \acronym{URL}s or email addresses. The default is
#'   \code{FALSE}. This setting interacts with \code{collapse_x}. See
#'   \sQuote{Details}.
#'
#' @param all_email a \code{logical} flag. If \code{TRUE}, individual
#'   email addresses are also picked up from mailto (or alias)
#'   \acronym{URL}s, and the remaining empty mailto \acronym{URL}s are
#'   discarded. This effectively forces \code{plain_email} to
#'   \code{TRUE}. If \code{FALSE} (the default), each mailto
#'   \acronym{URL} (possibly with multiple email addresses) is returned
#'   if \code{url_pattern} fits.
#'
#' @param collapse_x a \code{logical} flag. If \code{TRUE}, selected
#'   input strings are concatenated, separated by newline
#'   characters. This allows \acronym{URL}s in angle brackets and plain
#'   email addresses to extend across multiple strings. The default is
#'   \code{FALSE}: don't look across string boundaries.
#'
#' @param mailto_alias a \code{character} vector or
#'   \code{NULL}. Synonyms for mailto \acronym{URI} scheme. These are
#'   case insensitive and must follow the requirements for a
#'   \acronym{URI} scheme name: starts with an \acronym{ASCII} letter
#'   and is optionally followed by a sequence of letters, digits or
#'   characters in the set \code{"."}, \code{"+"}, \code{"-"}.
#'
#' @param scheme_sub a \code{list} of \acronym{URI} scheme
#'   substitutions to be made. The default is to substitute
#'   \code{"mailto"} for \code{"e-mail"} or \code{"email"} (copied from
#'   \code{mailto_alias}). Each element of the list corresponds to one
#'   official form, stored in the name of the element. The element is a
#'   \code{character} vector holding the unofficial forms. The strings
#'   are case-insensitive. Use \code{NULL} or an empty list for no
#'   substitutions.
#'
#' @param url_pattern a \code{character} string or \code{NULL}. Only
#'   strings matching this Perl-like regular expression are
#'   returned. This is not applied to plain email addresses (see
#'   \code{plain_email} and \code{email_pattern}). The matching is
#'   performed after all other processing. See \sQuote{Details}. If
#'   \code{NULL} or otherwise of zero \code{length}, no matching is
#'   done.
#'
#' @param email_pattern a \code{character} string or \code{NULL}. Like
#'   \code{url_pattern} but applied to email addresses.
#'
#' @param need_scheme a \code{logical} flag. If \code{TRUE}, return
#'   only strings starting with a technically valid, but not necessarily
#'   existing \acronym{URI} scheme followed by a \code{":"} (see
#'   \code{plain_email}). The default is \code{TRUE} if
#'   \code{url_pattern} is \code{\link{missing}} or empty, \code{FALSE}
#'   otherwise.
#'
#' @param deobfuscate a \code{logical} flag. If \code{TRUE} (the
#'   default), the function interprets some substrings with an
#'   \code{"at"} word (case insensitive) as email addresses. The actual
#'   pattern to match is more complicated, and false positives should be
#'   rare.
#'
#' @param rm_endpunct a \code{logical} flag or a \code{numeric} value
#'   with integral or infinite value. If \code{TRUE}, removes any
#'   \code{"."}, \code{"?"} or \code{"!"} that is suspected to end a
#'   sentence. Useful when no space has been used to separate the end
#'   punctuation from a \acronym{URL}. If \code{FALSE}, punctuation is
#'   not removed. A \code{numeric} value indicates the memory size,
#'   i.e. the maximum number of items (lines) across which a sentence
#'   may extend. A smaller number means faster operation. Numbers
#'   smaller than \code{1} are equivalent to \code{FALSE}, and
#'   \code{Inf} is equivalent to \code{TRUE}. The default is \code{20}.
#'
#' @return If \code{plain_email} is \code{FALSE}, returns a
#' \code{character} vector containing the \acronym{URL}s in
#' \code{x}. \acronym{URL} schemes are converted to lowercase, which
#' is the canonical form.
#'
#' If \code{plain_email} is \code{TRUE}, returns a \code{list} where
#' the first element \code{"url"} is the \acronym{URL} vector
#' described above and the second element \code{"email"} is a
#' \code{character} vector with the email addresses found. See
#' \code{all_email}.
#'
#' If non-ASCII results are present, their \code{\link{Encoding}} will
#' be "UTF-8".
#'
#' @examples
#'
#' email1 <- "user1@@example.org"
#' urls <- c("http://www.example.org/",
#'           "ftp://cran.r-project.org",
#'           "https://a,b,c@@[vf.a1,b2]/foo,bar",
#'           paste0("mailto:", email1))
#' phrase <- c(paste0("See ", urls[1], ", ", urls[2], " and"),
#'             paste0(urls[3], "."))
#' url_urls <- paste0("With prefix URL:", urls, " and that's all.")
#' comma_urls <- paste0(urls, collapse=",")
#' angle_urls <- sub(".", ".\n", paste0("<", urls, ">"), fixed=TRUE)
#' split_urls <- unlist(strsplit(angle_urls, "\n", fixed=TRUE))
#'
#' pu1 <- pick_urls(urls)
#' identical(pu1, urls[1:3])                       # TRUE
#' pu2 <- pick_urls(urls, url_pattern="")
#' identical(pu2, urls)                            # TRUE
#' pu3 <- pick_urls(phrase)
#' identical(pu3, pu1)                             # TRUE
#' pu4 <- pick_urls(url_urls, url_pattern="")
#' identical(pu4, urls)                            # TRUE
#' pu5 <- pick_urls(urls, url_pattern="", all_email=TRUE)
#' identical(pu5[["url"]], urls[1:3])              # TRUE
#' identical(pu5[["email"]], email1)               # TRUE
#' pu6 <- pick_urls(comma_urls, url_pattern="")
#' identical(pu6, urls)                            # TRUE
#' pu7 <- pick_urls(angle_urls, url_pattern="")
#' identical(pu7, urls)                            # TRUE
#' pu8 <- pick_urls(split_urls, url_pattern="", collapse_x=TRUE)
#' identical(pu8, urls)                            # TRUE
#'
#' emails <- c("user2 at example.org",
#'             "\"user 3\"(comment) @@ localhost",
#'             "\"user", " 4\"@@[::", " 1]")
#' emails_target <- c("user2@@example.org",
#'                    "\"user 3\"@@localhost",
#'                    "\"user 4\"@@[::1]")
#'
#' pe1 <- pick_urls(emails, plain_email=TRUE)
#' identical(pe1[["email"]], emails_target[1])     # TRUE
#' pe2 <- pick_urls(emails, plain_email=TRUE, email_pattern="")
#' identical(pe2[["email"]], emails_target[1:2])   # TRUE
#' pe3 <- pick_urls(emails, plain_email=TRUE, email_pattern="",
#'                  collapse_x=TRUE)
#' identical(pe3[["email"]], emails_target)        # TRUE
#' pe4 <- pick_urls(emails, plain_email=TRUE, email_pattern="",
#'                  deobfuscate=FALSE)
#' identical(pe4[["email"]], emails_target[2])     # TRUE
#'
#' @keywords utilities
#'
#' @references
#'
#' Berners-Lee, T., Fielding, R., and Masinter, L. (2005) Uniform
#' Resource Identifier (URI): Generic syntax. RFC 3986, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc3986.txt}.
#'
#' Braden, R., editor (1989) Requirements for Internet hosts -
#' application and support. RFC 1123, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc1123.txt}.
#'
#' Duerst, M., Masinter, L., and Zawinski, J. (2010) The 'mailto' URI
#' scheme. RFC 6068, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc6068.txt}.
#'
#' Duerst, M. and Suignard, M. (2005) Internationalized Resource
#' Identifiers (IRIs). RFC 3987, RFC
#' Editor. \url{https://www.rfc-editor.org/rfc/rfc3987.txt}.
#'
#' Elz, R. and Bush, R. (1997) Clarifications to the DNS
#' specification. RFC 2181, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc2181.txt}.
#'
#' Harrenstien, K., Stahl, M., and Feinler, E. (1985) DoD Internet
#' host table specification. RFC 952, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc952.txt}.
#'
#' Mockapetris, P. (1987) Domain names - concepts and facilities. RFC
#' 1034, RFC Editor.
#' \url{https://www.rfc-editor.org/rfc/rfc1034.txt}.
#'
#' R Core Team (2017) R: A language and environment for statistical
#' computing. R Foundation for Statistical Computing, Vienna, Austria.
#' \url{https://www.R-project.org/}.
#'
#' Resnick, P., editor (2008) Internet Message Format. RFC 5322, RFC
#' Editor.  \url{https://www.rfc-editor.org/rfc/rfc5322.txt}.
#'
#' @export
pick_urls <- function(x, plain_email = all_email, single_item = FALSE,
                      all_email = FALSE, collapse_x = FALSE,
                      mailto_alias = c("email", "e-mail"),
                      scheme_sub = list(mailto = mailto_alias),
                      url_pattern = "^(https?|ftp)://.",
                      email_pattern = "@[^.[]+\\.[^.]+",
                      need_scheme = missing(url_pattern) ||
                          !isTRUE(nzchar(url_pattern)),
                      deobfuscate = TRUE, rm_endpunct = 20) {
    ## Check arguments other than input strings --------------------------
    stopifnot(length(rm_endpunct) == 1L)
    if (is.logical(rm_endpunct)) {
        stopifnot(!is.na(rm_endpunct))
    } else if (is.numeric(rm_endpunct)) {
        stopifnot(!is.na(rm_endpunct), round(rm_endpunct) == rm_endpunct)
    } else {
        stop("'rm_endpunct' must be logical or numeric")
    }
    stopifnot(is.logical(collapse_x), length(collapse_x) == 1L,
              !is.na(collapse_x))
    url_pattern_exists <- length(url_pattern) > 0L
    if (url_pattern_exists) {
        stopifnot(is.character(url_pattern), length(url_pattern) == 1L,
                  !is.na(url_pattern), Encoding(url_pattern) != "bytes",
                  validEnc(url_pattern))
        url_pattern_exists <- nzchar(url_pattern)
    }
    stopifnot(is.logical(single_item), length(single_item) == 1L,
              !is.na(single_item), is.logical(all_email),
              length(all_email) == 1L, !is.na(all_email),
              is.logical(plain_email), length(plain_email) == 1L,
              !is.na(plain_email), is.logical(need_scheme),
              length(need_scheme) == 1L, !is.na(need_scheme))
    plain_email2 <- all_email || plain_email
    stopifnot(is.logical(deobfuscate), length(deobfuscate) == 1L,
              !is.na(deobfuscate))
    if (plain_email2) {
        email_pattern_exists <- length(email_pattern) > 0L
        if (email_pattern_exists) {
            stopifnot(is.character(email_pattern),
                      length(email_pattern) == 1L,
                      !is.na(email_pattern),
                      Encoding(email_pattern) != "bytes",
                      validEnc(email_pattern))
        }
    }
    ## String constants (regex fragments) --------------------------------
    ## ASCII letters
    k_ascii52 <- paste0(c(LETTERS, letters), collapse = "")
    ## URI scheme
    k_scheme <- paste0("[", k_ascii52, "][0123456789", k_ascii52, ".+-]*")
    k_sch_colon_noposs <- paste0(k_scheme, ":")
    k_sch_colon <- paste0(k_scheme, "+:")
    k_hex <- "[0123456789abcdefABCDEF]"
    k_latex <- paste0("\\\\[", k_ascii52, "]++\\{")
    ## Printable ASCII characters which must not appear bare in a
    ## URI. Also includes LaTeX macros: less spurious results if
    ## 'need_scheme' is FALSE and 'url_pattern' is permissive. Also
    ## includes "%" when not used as part of percent encoding.
    k_replace <- paste0(k_latex, "|[ \"<>\\\\^`{|}]|%(?!", k_hex, "{2})")
    k_replace_nsp <- sub(" ", "", k_replace, fixed = TRUE)
    ## Separator which does not interfere with email address matching
    k_mail_sep <- " \"\v "
    ## Folding white space (RFC 5322). As a change, "\r" is optional.
    k_fws <- "(?:[ \t]*+\r?+\n)?[ \t]++"
    k_fws_s <- "[ \t]*+(?:\r?+\n[ \t]++)*+"
    k_fws_p <- "(?:[ \t]++|[ \t]*+(?:\r?+\n[ \t]++)++)"
    k_fws_q <- "[ \t]*+(?:\r?+\n[ \t]++)?+"
    k_fws0 <- "[ \t]*+"
    k_fws0_n <- "[ \t]*+(?:\n[ \t]++)?+"
    k_fws1 <- "[ \t]++"
    k_fws1_n <- "[ \t]*+\n?[ \t]++"
    k_fws1s <- "^[ \t]++"
    ## Double quote (not escaped)
    k_quote <- "(?<!\\\\)\""
    ## Contents in quoted string
    k_qcont <- "(?:[^\"\\\\[:blank:][:space:][:cntrl:]]|\\\\.)"
    ## Quoted string
    k_qs <- paste0("\"(?:", k_fws_q, k_qcont, ")*+", k_fws_q, "\"")
    ##   a (possible) quote continues after the end of the string
    k_qs0 <- paste0("^\"(?:", k_fws0, k_qcont, ")*+", k_fws0, "$")
    ##   a possible quote continues from previous line, ends here
    k_qs1 <- paste0(k_fws1s, "(?:", k_qcont, k_fws0, ")*+\"")
    ##   a line in the middle of a possible quote
    k_qs2 <- paste0(k_fws1s, "(?:", k_qcont, k_fws0, ")*+",
                    k_fws0, "$")
    ## Begin and end comment in email
    k_lpar <- "(?<!\\\\)\\("
    k_rpar <- "(?<!\\\\)\\)"
    ## Domain name parts
    k_dns_nohyph <- "[^[:blank:][:space:][:cntrl:][:punct:]]"
    k_dns_text <- paste0("(?:", k_dns_nohyph, "|-)")
    ## Literal IP address (fragments), IPv6 and future. See is_host().
    k_dtext <- paste0("[^][/?", "#@\\\\[:blank:][:space:][:cntrl:]]")
    ##   the whole address
    k_lit <- paste0("\\[(?:", k_fws_q, k_dtext, ")*+", k_fws_q, "]")
    ##   address begins
    if (deobfuscate) {
        k_lit0 <-
            paste0("(?:",
                   paste0(c("(?<!@)@", "(?<![^) \t])[aA][tT]", "\\)"),
                          c(k_fws0_n, k_fws1_n, k_fws0_n), collapse="|"),
                   ")")
    } else {
        k_lit0 <- paste0("(?:(?<!@)@|\\))", k_fws0_n)
    }
    k_lit0 <- paste0(k_lit0, "\\[(?:", k_fws0, k_dtext, ")*+", k_fws0, "$")
    ##   address ends
    k_lit1 <- paste0(k_fws1s, "(?:", k_dtext, k_fws0, ")*+]")
    ##   address continues
    k_lit2 <- paste0(k_fws1s, "(?:", k_dtext, k_fws0, ")++",
                     k_fws0, "$")

    ## Length limits from RFC 2181. The total limit 255 characters
    ## should also be checked. The official limits may be smaller as
    ## they are actually octets (bytes).
    k_dns_label <- paste0(k_dns_nohyph, "(?:", k_dns_text, "{0,61}",
                          k_dns_nohyph, ")?+(?![[:alpha:][:digit:]])")
    if (plain_email2) {
        ## Quick test pattern for identifying potential email addresses
        k_rough_email0 <- "[^@]@[^@]"
        k_rough_email <- k_rough_email0
        k_rough_email1 <- "[^@]@(?:[^@]|$)"
        if (deobfuscate) {
            k_rough_email <-
                paste0(k_rough_email, "|(?:", k_fws, "|\\))[aA][tT]",
                       "(?:", k_fws, "|\\()")
            k_rough_email1 <-
                paste0(k_rough_email1, "|[) \t][aA][tT](?:[( \t]|$)")
        }
    }
    ## For rm_trailing()
    k_punct <- "[.?!]"
    k_word <- "[^[:blank:][:punct:] ]"
    k_end_sentence <- paste0(k_punct, "(?!", k_word, ")")
    k_before_word <-
        paste0("(?:^[[:space:][:punct:]]*+| ++)(?=", k_word, ")")
    k_proper <- paste0("(?: |^)[^[:blank:][:digit:]",
                       "[:punct:] ]++[[:punct:]]?+(?: |$)")
    ## Check arguments (continued) ---------------------------------------
    if (length(mailto_alias) > 0L) {
        stopifnot(is.character(mailto_alias))
        mail_alias <- mailto_alias[!is.na(mailto_alias)]
        mail_alias <- mail_alias[Encoding(mail_alias) != "bytes"]
        mail_alias <- mail_alias[validEnc(mail_alias)]
        mail_alias <- grep(paste0("^", k_scheme, "+$"), mail_alias,
                           value = TRUE, perl = TRUE)
        mail_alias <- tolower(mail_alias)
        mail_alias <- c("mailto", mail_alias)
    } else {
        mail_alias <- "mailto"
    }
    if (length(scheme_sub) > 0L) {
        stopifnot(is.list(scheme_sub))
        nonzero_sub <- which(lengths(scheme_sub, use.names = FALSE) > 0)
        if (length(nonzero_sub) > 0L) {
            stopifnot(vapply(scheme_sub[nonzero_sub],
                             is.character, FALSE, USE.NAMES = FALSE),
                      vapply(lapply(scheme_sub[nonzero_sub], validEnc),
                             all, FALSE, USE.NAMES = FALSE))
            official_sch <- names(scheme_sub)
            stopifnot(!is.null(official_sch),
                      !is.na(official_sch[nonzero_sub]),
                      validEnc(official_sch[nonzero_sub]),
                      nzchar(official_sch[nonzero_sub]))
            official_sch <- tolower(official_sch)
        }
    } else {
        nonzero_sub <- integer(0)
    }
    ## -------------------------------------------------------------------
    ## Helper functions follow. Some of them access variables of the
    ## main function, including arguments and string constants defined
    ## above. Look for "end of helper functions".

    ## rm_comments: Removes comments (in parenthesis) from
    ## 'string'. The format of comments is not as strict as instructed
    ## by RFC 5322. In the return value, "text" is 'string' with
    ## comments removed. "comment.loc" is comment locations (if any),
    ## with values between '0' and 'nchar(text)'. '0' means before the
    ## first remaining character, other 'k' means after the 'k':th
    ## character in 'text'. If comments were found, "orig.idx"
    ## contains a mapping from each character position in "text" to
    ## the corresponding location in 'string', "comments" contains the
    ## comments, and "comment.loc.orig" are comment locations in the
    ## original string. The last two items don't include the
    ## delimiting parenthesis.
    rm_comments <- function(string) {
        no_change <- list(text = string, comment.loc = numeric(0))
        lpar <- gregexpr(k_lpar, string, perl = TRUE)[[1L]]
        if (lpar[1L] == -1L) {
            return(no_change)
        }
        rpar <- gregexpr(k_rpar, string, perl = TRUE)[[1L]]
        if (rpar[1L] == -1L) {
            return(no_change)
        }
        n <- nchar(string)
        ## Exclude parentheses in a quoted string
        qs_loc <- gregexpr(k_qs, string, perl = TRUE)[[1L]]
        if (qs_loc[1L] != -1L) {
            not_qs <- rep.int(TRUE, n)
            qs_last <- qs_loc - 1 + attr(qs_loc, "match.length")
            for (k in seq_along(qs_loc)) {
                not_qs[qs_loc[k]:qs_last[k]] <- FALSE
            }
            lpar <- lpar[not_qs[lpar]]
            if (length(lpar) == 0L) {
                return(no_change)
            }
            rpar <- rpar[not_qs[rpar]]
        }
        rpar <- rpar[rpar > lpar[1L]]
        if (length(rpar) == 0L) {
            return(no_change)
        }
        balance <- numeric(n)
        for (k in seq_along(lpar)) {
            balance[lpar[k]:n] <- k
        }
        for (k in seq_along(rpar)) {
            tmp <- rpar[k]:n
            balance[tmp] <- balance[tmp] - 1
        }
        ## Exclude unbalanced closing parentheses
        first_neg <- which(balance < 0)[1L]
        while (!is.na(first_neg)) {
            tmp <- first_neg:n
            balance[tmp] <- balance[tmp] + 1
            first_neg <- which(balance < 0)[1L]
        }
        diff_balance <- c(balance[1L], diff(balance))
        ## zero: top-level comment ends
        zero <- which(balance == 0 & diff_balance == -1)
        n_comments <- length(zero)
        if (n_comments == 0L) {
            return(no_change)
        }
        ## one: top-level comment begins
        one <- which(balance == 1 & diff_balance == 1)
        ## Exclude unbalanced opening parentheses following comments
        one <- one[seq_len(n_comments)]
        comments <- substring(string, one + 1, zero - 1)
        keep_this <- rep.int(TRUE, n)
        for (k in seq_len(n_comments)) {
            keep_this[one[k]:zero[k]] <- FALSE
        }
        keep_subs <- substring(string, c(1, zero + 1), c(one - 1, n))
        string2 <- paste0(keep_subs, collapse = "")
        list(text = string2,
             comment_loc = cumsum(keep_this)[one],
             orig_idx = seq_len(n)[keep_this],
             comments = comments,
             comment.loc.orig = one + 1)
    } # end of rm_comments
    ## count_comments: Count the comment level (nestedness) at the end
    ## of the string. The argument 'level' is the level at the start
    ## of the string. See rm_comments.
    count_comments <- function(string, level = 0) {
        default <- list(level = 0, all.comment = FALSE,
                        end.comment = FALSE, begin.comment = FALSE)
        lpar <- gregexpr(k_lpar, string, perl = TRUE)[[1L]]
        if (lpar[1L] == -1L) {
            if (level == 0) {
                return(default)
            }
            lpar <- integer(0)
        }
        rpar <- gregexpr(k_rpar, string, perl = TRUE)[[1L]]
        if (rpar[1L] == -1L) {
            rpar <- integer(0)
        }
        n <- nchar(string)
        if (level > 0) {
            begin_comment <- grepl(k_fws1s, string, perl = TRUE)
            if (begin_comment) {
                level2 <- level
            } else {
                level2 <- 0
            }
        } else {
            begin_comment <- grepl(paste0(k_fws1s, "\\("), string,
                                   perl = TRUE)
            level2 <- 0
        }
        n2 <- n + level2
        lpar <- c(seq.int(to = 0, by = 1, length.out = level2), lpar)
        ## Exclude parentheses in a quoted string
        if (length(lpar) > 0L || length(rpar) > 0L) {
            qs_loc <- gregexpr(k_qs, string, perl = TRUE)[[1L]]
            if (qs_loc[1L] != -1L) {
                not_qs <- rep.int(TRUE, n2)
                qs_loc2 <- qs_loc + level2
                qs_last <- qs_loc2 - 1 + attr(qs_loc, "match.length")
                for (k in seq_along(qs_loc2)) {
                    not_qs[qs_loc2[k]:qs_last[k]] <- FALSE
                }
                lpar <- lpar[not_qs[lpar + level2]]
                if (level2 == 0 && length(lpar) == 0L) {
                    return(default)
                }
                rpar <- rpar[not_qs[rpar + level2]]
            }
            if (length(lpar) > 0L) {
                rpar <- rpar[rpar > lpar[1L]]
                lpar <- lpar + level2
            }
            rpar <- rpar + level2
        }
        balance <- numeric(n2)
        for (k in seq_along(lpar)) {
            balance[lpar[k]:n2] <- k
        }
        for (k in seq_along(rpar)) {
            tmp <- rpar[k]:n2
            balance[tmp] <- balance[tmp] - 1
        }
        ## Exclude unbalanced closing parentheses
        first_neg <- which(balance < 0)[1L]
        unbalanced <- logical(n2)
        while (!is.na(first_neg)) {
            unbalanced[first_neg] <- TRUE
            tmp <- first_neg:n2
            balance[tmp] <- balance[tmp] + 1
            first_neg <- which(balance < 0)[1L]
        }
        unbalanced <- which(unbalanced)
        if (length(unbalanced) > 0L) {
            rpar <- rpar[!(rpar %in% unbalanced)]
        }
        new_level <- balance[n2]
        b0 <- which(balance == 0)
        n_rpar <- length(rpar)
        all_comment <- if (level > 0 || new_level > 0) {
            NA
        } else {
            ## n_rpar > 0 is TRUE in this branch
            b0 <- b0[!(b0 %in% rpar)]
            if (length(b0) > 0L) {
                all(diff(b0[b0 < rpar[n_rpar]]) > 1L) &&
                    all(substring(string, b0, b0) %in% c(" ", "\t"))
            } else {
                TRUE
            }
        }
        end_comment <- if (isTRUE(all_comment)) {
            TRUE
        } else if (new_level == 0) {
            if (n_rpar > 0L) {
                grepl("^[ \t]*+$",
                      substr(string, rpar[n_rpar] - level2 + 1, n),
                      perl = TRUE)
            } else {
                FALSE
            }
        } else {
            TRUE
        }
        list(level = new_level, all.comment = all_comment,
             end.comment = end_comment, begin.comment = begin_comment)
    } # end of count_comments
    ## is_host: For each item in 'strings', is (a prefix of) it an RFC
    ## 3986 valid(ish) host part in a URL. Returns a numeric vector
    ## the same length as 'strings', with items: 0, when there is no
    ## substring starting at the beginning of the string that matches
    ## the rules, or when a hostname is too long; Inf, when the whole
    ## string is a match (except when the string has length 0);
    ## positive integer giving the length of the prefix that matches.
    is_host <- function(strings) {
        nc <- nchar(strings)
        n <- length(strings)
        result <- numeric(n)
        k_dns <- paste0("^", k_dns_label, "(?:\\.", k_dns_label, ")*+\\.?+")
        match_dns <- regexpr(k_dns, strings, perl = TRUE)
        is_dns <- match_dns == 1L
        if (any(is_dns)) {
            which_dns <- which(is_dns)
            result[which_dns] <- Inf
            match_len <- attr(match_dns, "match.length")[which_dns]
            sub_match <- match_len < nc[which_dns]
            result[which_dns[sub_match]] <- match_len[sub_match]
            result[which_dns[match_len > 255L]] <- 0
            if (length(which_dns) == n) {
                return(result)
            }
            work_flag <- !is_dns
            work <- which(work_flag)
            work_set <- strings[work]
            n_work <- length(work)
        } else {
            work_flag <- rep.int(TRUE, n)
            work <- seq_len(n)
            work_set <- strings
            n_work <- n
        }
        k_fut_char <- paste0("[", k_ascii52,
                             "0123456789._~:!$&'()*+,;=-]")
        k_future <- paste0("^\\[v", k_hex, "++\\.", k_fut_char, "++]")
        k_h16 <- paste0(k_hex, "{1,4}+")
        k_dec_oct <-
            "(?:[01]?[0123456789]{1,2}+|2[01234][0123456789]|25[012345])"
        k_ip4 <- paste0(k_dec_oct, "(?:\\.", k_dec_oct, "){3}")
        k_ls32 <- paste0("(?:", k_h16, ":", k_h16, "|", k_ip4, ")")
        k_h16_col <- paste0("(?:", k_h16, ":)")
        k_temp <- paste0("(?:", k_h16_col)
        k_ip6 <-
            paste0("^\\[",
                   c(paste0(k_h16_col, "{6}", k_ls32),
                     paste0("::", k_h16_col, "{5}", k_ls32),
                     paste0(k_hex, "{0,4}+::", k_h16_col, "{4}", k_ls32),
                     paste0(k_temp, "?", k_h16, ")?+::",
                            k_h16_col, "{3}", k_ls32),
                     paste0(k_temp, "{0,2}", k_h16, ")?+::",
                            k_h16_col, "{2}", k_ls32),
                     paste0(k_temp, "{0,3}", k_h16, ")?+::",
                            k_h16_col, k_ls32),
                     paste0(k_temp, "{0,4}", k_h16, ")?+::", k_ls32),
                     paste0(k_temp, "{0,5}", k_h16, ")?+::", k_h16),
                     paste0(k_temp, "{0,6}", k_h16, ")?+::")), "]")
        for (regex in c(k_ip6, k_future)) {
            match_this <- regexpr(regex, work_set, perl = TRUE)
            match_found <- match_this == 1L
            if (any(match_found)) {
                which_match <- work[match_found]
                result[which_match] <- Inf
                match_len <- attr(match_this, "match.length")[match_found]
                sub_match <- match_len < nc[which_match]
                result[which_match[sub_match]] <- match_len[sub_match]
                if (length(which_match) == n_work) {
                    return(result)
                }
                work_flag[work[match_found]] <- FALSE
                work <- which(work_flag)
                work_set <- strings[work]
                n_work <- length(work)
            }
        }
        result
    } # end of is_host
    find_ip_literal <- function(string, allow_space) {
        if (!grepl("[", string, fixed = TRUE) ||
             !grepl("]", string, fixed = TRUE)) {
            return(NULL)
        }
        if (allow_space) {
            lit_loc <- gregexpr("\\[[^]]++]", string, perl = TRUE)[[1L]]
        } else {
            lit_loc <- gregexpr("\\[[^][:space:]]++]", string,
                                perl = TRUE)[[1L]]
        }
        lit_last <- lit_loc - 1 + attr(lit_loc, "match.length")
        lit_subs <- substring(string, lit_loc, lit_last)
        if (allow_space) {
            lit_subs <- gsub("[[:space:]]+", "", lit_subs, perl = TRUE)
        }
        keep_loc <- is.infinite(vapply(lit_subs, is_host, 0,
                                       USE.NAMES = FALSE))
        if (any(keep_loc)) {
            list(lit_loc[keep_loc], lit_last[keep_loc])
        } else {
            NULL
        }
    }
    ## rev_brackets: Replaces brackets (), [] with the other character
    ## in the pair, then reverses the order of characters.
    rev_brackets <- function(string) {
        chars <- strsplit(string, "")[[1L]]
        tmp <- chars == "("
        chars[chars == ")"] <- "("
        chars[tmp] <- ")"
        tmp <- chars == "["
        chars[chars == "]"] <- "["
        chars[tmp] <- "]"
        paste0(rev(chars), collapse="")
    }
    ## find_fixed_perl: Find perl_pat (perl=TRUE) which ends with
    ## fixed_pat (fixed=TRUE). peek_back > 0 can be used when perl_pat
    ## has the same conditions for all character before fixed_pat.
    find_fixed_perl <- function(string, fixed_pat, perl_pat,
                                peek_back = 50) {
        remain <- string
        skip <- 0
        nc <- nchar(remain)
        nc_fixed <- nchar(fixed_pat)
        nc_fixed_m1 <- nc_fixed - 1
        do_peek <- peek_back > 0
        while (nc > nc_fixed) {
            loc1 <- regexpr(fixed_pat, remain, fixed = TRUE)
            if (loc1 == -1) {
                break
            }
            newskip <- loc1 + nc_fixed_m1
            start1 <- 1
            if (do_peek) {
                start1 <- max(1, loc1 - peek_back)
            }
            sub1 <- substr(remain, start1, newskip)
            loc2 <- regexpr(perl_pat, sub1, perl = TRUE)
            if (loc2 != -1) {
                if (do_peek) {
                    if (loc2 == 1) {
                        sub1 <- substr(remain, 1, newskip)
                        loc2 <- regexpr(perl_pat, sub1, perl = TRUE)
                    } else {
                        loc2 <- loc2 - 1 + start1
                    }
                }
                ## match.length is preserved
                return(loc2 + skip)
            }
            skip <- skip + newskip
            remain <- substr(remain, newskip + 1, nc)
            nc <- nc - newskip
        }
        -1
    } # end of find_fixed_perl
    ## split_and_check: Handles commas (keeps some of them, splits the
    ## string at others). Removes a possible (repeated) "URL:"
    ## prefix. Discards URLs with invalid host or port
    ## sections. Detects some illegal patterns in the path, query and
    ## fragment sections, and splits the URL there. See the
    ## requirements set in the comments of pick_one_url().
    split_and_check <- function(string, cut_comma_email, split_userinfo,
                                remaining = FALSE, only_first = FALSE) {
        k_url_auth <- paste0(k_sch_colon, "//")
        k_comma_sch <- paste0(",", k_sch_colon)
        strings <- character(0)
        ## paste / break before "#" to circumvent parse error when
        ## indenting in ESS (version 13.09-1)
        k_end_auth <- paste0("[/?", "#]")
        k_not_uinfo <- paste0("[][/?", "#]")
        k_break <- paste0(",$|(?<=/),(?![,/?", "#])|,[([]*+", k_sch_colon)
        ## Iterate through the string. 'remain'ing part gets shorter
        ## on every round.
        remain <- string
        while (nzchar(remain) && (!only_first || length(strings) == 0L)) {
            first_scheme <- find_fixed_perl(remain, ":", k_sch_colon)
            ## Remove any (repeated) "URL:" (case insensitive) prefix
            start_url <- first_scheme != -1L &&
                grepl("^[Uu][Rr][Ll]$",
                      substr(remain, first_scheme,
                             first_scheme - 2 + attr(first_scheme,
                                                     "match.length")),
                      perl = TRUE)
            if (!only_first && start_url) {
                remain <- paste0(substr(remain, 1L, first_scheme - 1L),
                                 sub("^(?:[Uu][Rr][Ll]:)+", "",
                                     substr(remain, first_scheme + 4,
                                            nchar(remain)), perl = TRUE))
                if (!nzchar(remain)) {
                    break
                }
                first_scheme <- find_fixed_perl(remain, ":", k_sch_colon)
            }
            if (first_scheme == -1L) {
                ## Simple case: no URL schemes => split at every comma
                split1 <- strsplit(remain, ",", fixed = TRUE)[[1L]]
                if (only_first) {
                    remain <- paste0(split1[-1L], collapse = ",")
                    split1 <- split1[1L]
                } else {
                    remain <- ""
                    split1 <- split1[nzchar(split1)]
                }
                strings <- c(strings, split1)
                split1 <- NULL
                break
            } else if (only_first && start_url) {
                split2 <- strsplit(substr(remain, first_scheme,
                                          nchar(remain)),
                                   ",", fixed = TRUE)[[1L]]
                remain <- paste0(split2[-1L], collapse = ",")
                strings <- c(strings, "")
                split2 <- NULL
                break
            }
            url_scheme <- substr(remain, first_scheme,
                                 first_scheme - 2 +
                                 attr(first_scheme, "match.length"))
            if (first_scheme > 1L) {
                ## Handle commas preceding the presumed URL scheme
                pre_scheme <- substr(remain, 1L, first_scheme - 1L)
                commas <- gregexpr(",", pre_scheme, fixed = TRUE)[[1L]]
                if (commas[1L] != -1L) {
                    n_commas <- length(commas)
                    sub_commas <- substr(remain, 1L, commas[n_commas])
                    split3 <- strsplit(sub_commas, ",", fixed=TRUE)[[1L]]
                    remain <- substr(remain, commas[n_commas] + 1,
                                     nchar(remain))
                    if (only_first) {
                        n_split3 <- length(split3)
                        if (n_split3 > 1L) {
                            remain <- paste(paste0(split3[-1L],
                                                   collapse = ","),
                                            remain, sep = ",")
                        }
                        split3 <- split3[1L]
                    } else {
                        split3 <- split3[nzchar(split3)]
                    }
                    strings <- c(strings, split3)
                    split3 <- NULL
                    if (only_first) {
                        break
                    }
                    first_scheme <- first_scheme - commas[n_commas]
                }
            }
            if (first_scheme > 1L) {
                prefix <- substr(remain, 1L, first_scheme - 1L)
            } else {
                prefix <- ""
            }
            auth_loc <- find_fixed_perl(remain, "://", k_url_auth)
            if (auth_loc == first_scheme) {
                ## A. URL is of form "foo://", i.e. contains authority
                ## part.  'point' points to location in 'remain'.
                point <- auth_loc + attr(auth_loc, "match.length")
                n_rem <- nchar(remain)
                p_fwd <- substr(remain, point, n_rem)
                userinfo <- FALSE
                ## Locate the authority part (new)
                host_len <- is_host(p_fwd)
                if (host_len <= 0) {
                    ## Skip userinfo if present
                    at1 <- regexpr("@", p_fwd, fixed = TRUE)
                    if (at1 != -1L &&
                        !grepl(k_not_uinfo, substr(p_fwd, 1L, at1 - 1L),
                               perl = TRUE)) {
                        point <- point + at1
                        p_fwd <- substr(remain, point, n_rem)
                        host_len <- is_host(p_fwd)
                        userinfo <- TRUE
                    }
                }
                look_for_host <- TRUE
                break_found <- FALSE
                while (look_for_host && is.finite(host_len)) {
                    hl_p1 <- point + host_len
                    after_auth <- substr(remain, hl_p1, hl_p1)
                    if (after_auth == ":") {
                        port <- substr(remain, hl_p1 + 1, n_rem)
                        port_len <-
                            attr(regexpr("^[0123456789]*", port,
                                         perl = TRUE), "match.length")
                        hl_p1 <- hl_p1 + port_len + 1
                        after_auth <- substr(remain, hl_p1, hl_p1)
                    }
                    if (grepl(k_end_auth, after_auth, perl = TRUE)) {
                        point <- hl_p1
                        p_fwd <- substr(remain, point, n_rem)
                    } else {
                        ## (Heuristically) consider the
                        ## possibility that this part of the
                        ## string belongs in userinfo
                        at2 <- hl_p1 - 1 +
                            regexpr("@", substr(remain, hl_p1, n_rem),
                                    fixed = TRUE)
                        add_string <- TRUE
                        if (userinfo && at2 >= hl_p1) {
                            ## Only one userinfo allowed
                            if (at2 == hl_p1) {
                                next_comma <- at2 +
                                    regexpr(",",
                                            substr(remain, at2 + 1,
                                                   n_rem), fixed = TRUE)
                                if (next_comma > at2) {
                                    hl_p1 <- next_comma
                                } else {
                                    hl_p1 <- n_rem + 1
                                }
                                add_string <- FALSE
                            }
                        } else if (!userinfo && at2 >= hl_p1) {
                            sub_at <- substr(remain, point, at2 - 1)
                            if (!grepl(k_not_uinfo, sub_at, perl=TRUE) &&
                                 (!split_userinfo ||
                                   !grepl(k_comma_sch, sub_at,
                                          perl = TRUE))) {
                                point <- at2 + 1
                                p_fwd <- substr(remain, point, n_rem)
                                host_len <- is_host(p_fwd)
                                userinfo <- TRUE
                                next
                            }
                        }
                        if (add_string) {
                            strings <- c(strings,
                                         substr(remain, 1, hl_p1 - 1))
                        }
                        remain <- substr(remain, hl_p1, n_rem)
                        break_found <- TRUE
                    }
                    look_for_host <- FALSE
                }
                if (break_found) {
                    next
                }
                if (!is.finite(host_len)) {
                    point <- n_rem + 1
                    p_fwd <- ""
                }
            } else {
                ## B. URL does not contain an authority part
                point <- first_scheme + attr(first_scheme, "match.length")
                n_rem <- nchar(remain)
                p_fwd <- substr(remain, point, n_rem)
            }
            ## Split if square brackets are found but not as
            ## delimiters of a literal IP address
            break_point <- -1L
            left_loc <- gregexpr("[", p_fwd, fixed = TRUE)[[1L]]
            if (left_loc[[1L]] == -1L) {
                left_loc <- integer(0)
            }
            right_loc <- gregexpr("]", p_fwd, fixed = TRUE)[[1L]]
            if (right_loc[[1L]] == -1L) {
                right_loc <- integer(0)
            }
            if (length(left_loc) > 0L || length(right_loc) > 0L) {
                lit_loc <- gregexpr(k_lit, p_fwd, perl = TRUE)[[1L]]
                if (lit_loc[[1L]] == -1L) {
                    break_point <- min(left_loc, right_loc)
                } else {
                    lit_last <-
                        lit_loc - 1 + attr(lit_loc, "match.length")
                    lit_sub <- substring(p_fwd, lit_loc, lit_last)
                    h_good <- is.infinite(is_host(lit_sub))
                    if (!any(h_good)) {
                        break_point <- min(left_loc, right_loc)
                    } else {
                        lit_loc <- lit_loc[h_good]
                        lit_last <- lit_last[h_good]
                        h_flag <- logical(nchar(p_fwd))
                        for (l_idx in seq_along(lit_loc)) {
                            h_flag[lit_loc[l_idx]:lit_last[l_idx]] <- TRUE
                        }
                        left_loc <- left_loc[!h_flag[left_loc]]
                        right_loc <- right_loc[!h_flag[right_loc]]
                        if (length(left_loc) > 0L ||
                            length(right_loc) > 0L) {
                            break_point <- min(left_loc, right_loc)
                        }
                    }
                }
            }
            ## Split if more than one fragment component (forbidden)
            break_point2 <- regexpr("#[^#]*+#", p_fwd, perl = TRUE)
            if (break_point2 != -1L) {
                break_point2 <-
                    break_point2 - 1 + attr(break_point2, "match.length")
                if (break_point == -1L) {
                    break_point <- break_point2
                } else {
                    break_point <- min(break_point, break_point2)
                }
            }
            if (!only_first || cut_comma_email) {
                commas <- gregexpr(",", p_fwd, fixed = TRUE)[[1L]]
                any_commas <- commas[1L] != -1L
                nc_fwd <- n_rem - point + 1L
            }
            if (!only_first) {
                ## Heuristic method: split if comma in path (?query,
                ## #fragment) part is either:
                ## * the final character (eats a trailing comma),
                ## * immediately preceded by a slash "/" but not followed
                ##   by [,/?#]
                ## * immediately followed by optional brackets and what
                ##   looks like a URI scheme, or...
                break_point3 <- regexpr(k_break, p_fwd, perl = TRUE)[[1L]]
                if (break_point3 != -1L && break_point == -1L) {
                    break_point <- break_point3
                } else if (break_point3 != -1L) {
                    break_point <- min(break_point, break_point3)
                }
                ## above, perl = TRUE needed for lookbehind and lookahead
                if (nzchar(prefix)) {
                    ## * ... immediately preceded by rev_brackets(prefix)
                    break_pat <- paste0(rev_brackets(prefix), ",")
                    break_point4 <-
                        regexpr(break_pat, p_fwd, fixed = TRUE)[[1L]]
                    if (break_point4 != -1L) {
                        break_point4 <- break_point4 + nchar(prefix)
                        if (break_point == -1L) {
                            break_point <- break_point4
                        } else {
                            break_point <- min(break_point, break_point4)
                        }
                    }
                }
                ## Another heuristic: split if comma is followed by a
                ## hostname meeting a few conditions
                if (any_commas) {
                    k_min_chars <- 4L
                    k_dotname <- "[^.]\\.[^.]{2,}+$"
                    commas2 <- commas[commas < nc_fwd - k_min_chars + 1L]
                    if (break_point != -1L) {
                        commas2 <- commas2[commas2 < break_point]
                    }
                    for (cloc in commas2) {
                        this_sub <- substr(p_fwd, cloc + 1, nc_fwd)
                        ih <- is_host(this_sub)
                        if (ih <= 0) {
                            next
                        }
                        if (is.infinite(ih)) {
                            hostname <- this_sub
                        } else {
                            if (!(substr(this_sub, ih + 1, ih + 1) %in%
                                  c("/", ","))) {
                                next
                            }
                            hostname <- substr(this_sub, 1, ih)
                        }
                        if (grepl(k_dotname, hostname, perl=TRUE) &&
                            grepl("^(?:www|ftp|mail|news|smtp|pop|imap)",
                                  hostname, ignore.case=TRUE,perl=TRUE)) {
                            break_point <- cloc
                            break
                        }
                    }
                }
            }
            if (cut_comma_email && any_commas) {
                max_at <- max(gregexpr("@", p_fwd, fixed = TRUE)[[1L]])
                commas <- commas[commas < max_at]
                if (break_point != -1L) {
                    commas <- commas[commas < break_point]
                }
                if (length(commas) > 0L &&
                    tolower(url_scheme) %in% mail_alias) {
                    question <- regexpr("?", p_fwd, fixed = TRUE)
                    if (question == -1L) {
                        commas <- NULL
                    } else {
                        commas <- commas[commas > question]
                    }
                }
                for (cloc in commas) {
                    this_sub <- substr(p_fwd, cloc + 1, nc_fwd)
                    this_mail <-
                        pick_start_email(this_sub, deobfuscate = FALSE,
                                         allow_comments = FALSE)
                    if (nzchar(this_mail)) {
                        break_point <- cloc
                        break
                    }
                }
            }
            if (break_point == -1L) {
                strings <- c(strings, remain)
                remain <- ""
            } else {
                break_point <- break_point - 1 + point
                strings <- c(strings, substr(remain, 1, break_point - 1))
                remain <- substr(remain, break_point, n_rem)
            }
        }
        if (only_first && length(strings) == 0L) {
            strings <- ""
        }
        if (remaining) {
            list(strings, remain)
        } else {
            strings
        }
    } # end of split_and_check
    ## Usage contract: don't have loc pointing to an item in orig_idx
    ## which is immediately to the right (when type_start is TRUE) or
    ## left (when type_start is FALSE) from an NA item
    restore_parts <- function(orig_idx, loc, max_idx, type_start) {
        n_idx <- length(orig_idx)
        flag_under <- loc < 1
        flag_over <- loc > n_idx
        tmp_loc <- loc
        tmp_loc[flag_under | flag_over] <- NA_integer_
        result <- orig_idx[tmp_loc]
        flag_na <- is.na(result)
        flag_na[is.na(tmp_loc)] <- FALSE
        any_na <- any(flag_na)
        if (type_start) {
            idx_1 <- which(tmp_loc == 1)
            tmp_loc[idx_1] <- NA_integer_
            probe <- orig_idx[tmp_loc - 1]
            idx_fix <- which(probe < result - 1)
            result[idx_fix] <- probe[idx_fix] + 1
            result[idx_1] <- 1
            result[flag_over] <- orig_idx[n_idx] + 1
            result[flag_under] <- 1
            if (any_na) {
                idx_na <- tmp_loc[flag_na]
                not_na_sum <- cumsum(!is.na(orig_idx))
                tmp_sum <- not_na_sum[idx_na]
                tmp_idx <- match(tmp_sum, not_na_sum)
                tmp_result <- orig_idx[tmp_idx] + 1
                tmp_result[tmp_sum == 0L] <- 1
                result[flag_na] <- tmp_result
            }
        } else {
            probe <- orig_idx[tmp_loc + 1]
            idx_fix <- which(probe > result + 1)
            result[idx_fix] <- probe[idx_fix] - 1
            result[tmp_loc == n_idx] <- max_idx
            result[flag_over] <- max_idx
            result[flag_under] <- orig_idx[1L] - 1
            if (any_na) {
                idx_na <- tmp_loc[flag_na]
                not_na_sum <- cumsum(!is.na(orig_idx))
                tmp_idx <- match(not_na_sum[idx_na] + 1, not_na_sum)
                tmp_result <- orig_idx[tmp_idx] - 1
                tmp_result[is.na(tmp_result)] <- max_idx
                result[flag_na] <- tmp_result
            }
        }
        result
    }
    ## remove_overlap: When given match locations and lengths, returns
    ## disjoint (non-overlapping) matches, sorted from left to right.
    remove_overlap <- function(loc, len, ...) {
        n <- length(loc)
        if (n < 2L) {
            return(c(list(loc, len), list(...)))
        }
        work <- order(loc, -len)
        loc2 <- loc[work]
        last <- loc2 - 1 + len[work]
        used <- logical(max(last))
        keep <- rep.int(TRUE, n)
        ## Advance from left to right in the string. Secondary sort
        ## criterion: length of substring, decreasing order. Each
        ## match to be kept invalidates any later match spanning any
        ## of the same character locations.
        for (k in seq_len(n)) {
            span <- loc2[k]:last[k]
            if (any(used[span])) {
                keep[k] <- FALSE
            } else {
                used[span] <- TRUE
            }
        }
        work <- work[keep]
        c(list(loc[work], len[work]), lapply(list(...), `[`, work))
    }
    ## pick_email: Finds (obfuscated) email addresses in
    ## 'strings'. Addresses that are also part of a URL are not
    ## returned. Returns a list containing 1. (deobfuscated)
    ## addresses, 2. 'strings' after the addresses have been removed.
    pick_email <- function(strings, single_email, url_check, deobfuscate,
                           allow_comments = TRUE) {
        ## Sources for (dis)allowed characters: RFCs 952, 1123, 3986,
        ## 5322. Building regular expressions for email addresses.
        k_atext <- "[^][()<>:;@\\\\,.\"[:blank:][:space:][:cntrl:]]"
        ## Dot-atoms
        k_dot_atoms <- c(paste0(k_atext, "++(?:\\.", k_atext, "++)*+"),
                         paste0(k_dns_label, "(?:\\.", k_dns_label, ")",
                                c("*", "+")))
        k_addr_begin <- paste0("(", c(k_qs, k_dot_atoms[1L]), ")")
        k_begin_quot <- c(TRUE, FALSE)
        ## To reduce the number of false positives, the domain part of
        ## an obfuscated address must match a stricter pattern. Allow
        ## several folding white space sequences to make comment
        ## handling simpler.
        k_addr_end <- paste0(k_fws_s, "@", k_fws_s, "(",
                             c(k_dot_atoms[2L], k_lit), ")")
        k_end_dns <- c(TRUE, FALSE)
        k_end_at <- c(TRUE, TRUE)
        k_rough_comment <- "\\)[[:space:]]*+@[^@]|[^@]@[[:space:]]*+\\("
        if (deobfuscate) {
            k_addr_end <- c(k_addr_end,
                            paste0(k_fws_p, "[aA][tT]", k_fws_p, "(",
                                   c(k_dot_atoms[3L], k_lit), ")"))
            k_end_dns <- c(k_end_dns, TRUE, FALSE)
            k_end_at <- c(k_end_at, FALSE, FALSE)
            k_rough_comment <-
                paste0(k_rough_comment,
                       "|\\)[[:space:]]*+[aA][tT][([:space:]]",
                       "|[)[:space:]][aA][tT][[:space:]]*+\\(")
            k_nopunct <- "^(?:[^[:punct:]]|[-.])++$"
        }
        k_letter <- "[^[:blank:][:space:][:cntrl:][:punct:][:digit:]]"
        k_address <-
            vapply(k_addr_begin, paste0, character(length(k_addr_end)),
                   k_addr_end, USE.NAMES = FALSE)
        k_addr_quot <- rep(k_begin_quot, each = length(k_addr_end))
        k_addr_dns <- rep.int(k_end_dns, length(k_addr_begin))
        k_addr_at <- rep.int(k_end_at, length(k_addr_begin))
        if (is.character(url_check)) {
            schemes_allowed <- url_check
            url_check2 <- TRUE
        } else {
            schemes_allowed <- character(0)
            url_check2 <- url_check
        }
        if (url_check2) {
            tmp <- remove_urls(strings, schemes_allowed = schemes_allowed,
                               deobfuscate = deobfuscate)
            strings2 <- tmp[[1L]]
            url_idx <- tmp[[2L]]
            tmp <- NULL
            has_urls <- strings2 != strings
        } else {
            strings2 <- strings
            url_idx <- NULL
            has_urls <- logical(length(strings))
        }
        remain <- strings
        visiting <- seq_along(strings)
        visit_id <- visiting
        work_stack <- list(list(strings = strings2, visit = visiting))
        n_stack <- 0
        ## On the first step, inspect the whole string. On each
        ## possible further step, inspect one level of (nested)
        ## comments in parentheses.
        while (length(work_stack) > n_stack) {
            n_stack <- length(work_stack)
            stack_n <- work_stack[[n_stack]]
            strings2 <- stack_n[["strings"]]
            n_strings <- length(strings2)
            if (single_email) {
                eml <- character(n_strings)
            } else {
                eml <- rep.int(list(character(0)), n_strings)
            }
            reg_addr <- rep.int(list(numeric(0)), n_strings)
            match_len <- reg_addr
            host_loc <- reg_addr
            local_len <- reg_addr
            host_is_dns <- rep.int(list(logical(0)), n_strings)
            if (allow_comments) {
                rm_idx <- grep("(", strings2, fixed = TRUE)
                if (length(rm_idx) > 0L) {
                    rm_idx <- rm_idx[grep(")", strings2[rm_idx],
                                          fixed = TRUE)]
                    if (length(rm_idx) > 0L) {
                        rm_idx <- rm_idx[grep(k_rough_comment,
                                              strings2[rm_idx],
                                              perl = TRUE)]
                    }
                }
                n_rm <- length(rm_idx)
                if (n_rm > 0L) {
                    if (n_rm == n_strings) {
                        no_com <- lapply(strings2, rm_comments)
                        strings2 <- vapply(no_com, `[[`, "", 1L,
                                           USE.NAMES=FALSE)
                    } else {
                        no_com_tmp <-
                            lapply(strings2[rm_idx], rm_comments)
                        strings2[rm_idx] <-
                            vapply(no_com_tmp, `[[`, "", 1L,
                                   USE.NAMES=FALSE)
                        no_com <-
                            rep.int(list(list(NULL, NULL)), n_strings)
                        no_com[rm_idx] <- no_com_tmp
                        no_com_tmp <- NULL
                    }
                    any_comments <- TRUE
                } else {
                    any_comments <- FALSE
                }
            } else {
                any_comments <- FALSE
            }
            ## Reduce complexity of regular expression by matching
            ## multiple patterns
            if (any(nzchar(strings2))) {
                idx_brac <- grep("[", strings2, fixed = TRUE)
                idx_quot <- grep("\"", strings2, fixed = TRUE)
                idx_brac_quot <- intersect(idx_brac, idx_quot)
                idx_at <- grep("@", strings2, fixed = TRUE)
                if (deobfuscate) {
                    idx_obfu <- grep(paste0(k_fws, "[aA][tT]", k_fws),
                                     strings2, perl = TRUE)
                }
                reg0 <- rep.int(list(-1L), n_strings)
                for (k in seq_along(k_address)) {
                    this_addr <- k_address[k]
                    this_lit <- !k_addr_dns[k]
                    this_quot <- k_addr_quot[k]
                    this_at <- k_addr_at[k]
                    reg_this <- reg0
                    if (this_lit && this_quot) {
                        if (this_at) {
                            work <- intersect(idx_brac_quot, idx_at)
                        } else {
                            work <- intersect(idx_brac_quot, idx_obfu)
                        }
                    } else if (this_lit) {
                        if (this_at) {
                            work <- intersect(idx_brac, idx_at)
                        } else {
                            work <- intersect(idx_brac, idx_obfu)
                        }
                    } else if (this_quot) {
                        if (this_at) {
                            work <- intersect(idx_quot, idx_at)
                        } else {
                            work <- intersect(idx_quot, idx_obfu)
                        }
                    } else if (this_at) {
                        work <- idx_at
                    } else {
                        work <- idx_obfu
                    }
                    if (length(work) > 0L) {
                        reg_this[work] <-
                            gregexpr(this_addr, strings2[work],
                                     perl = TRUE)
                    } else {
                        next
                    }
                    hits <- which(vapply(reg_this, `[`, -1L, 1L,
                                         USE.NAMES = FALSE) != -1L)
                    n_hits <- length(hits)
                    if (n_hits == 0L) {
                        next
                    }
                    reg_this <- reg_this[hits]
                    n_match <- lengths(reg_this, use.names = FALSE)
                    host_is_dns[hits] <-
                        mapply(c, host_is_dns[hits],
                               lapply(n_match, rep.int, x=k_addr_dns[k]),
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                    reg_addr[hits] <-
                        mapply(c, reg_addr[hits], reg_this,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                    match_len[hits] <-
                        mapply(c, match_len[hits],
                               lapply(reg_this, attr, "match.length"),
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                    dom_s <- vector(mode = "list", length = n_hits)
                    loc_l <- dom_s
                    for (l in seq_len(n_hits)) {
                        rt_l <- reg_this[[l]]
                        dom_s[[l]] <- attr(rt_l, "capture.start")[, 2L]
                        loc_l[[l]] <- attr(rt_l, "capture.length")[, 1L]
                    }
                    host_loc[hits] <-
                        mapply(c, host_loc[hits], dom_s,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                    local_len[hits] <-
                        mapply(c, local_len[hits], loc_l,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                }
                firm <- mapply(remove_overlap, reg_addr, match_len,
                               host_loc, local_len, host_is_dns,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                reg_addr <- lapply(firm, `[[`, 1L)
                match_len <- lapply(firm, `[[`, 2L)
                host_loc <- lapply(firm, `[[`, 3L)
                local_len <- lapply(firm, `[[`, 4L)
                host_is_dns <- lapply(firm, `[[`, 5L)
            }
            idx_addr <- which(lengths(reg_addr, use.names = FALSE) > 0L)
            n_idx <- length(idx_addr)
            if (any_comments) {
                comment_loc <- lapply(no_com, `[[`, 2L)
                n_comments <- lengths(comment_loc, use.names = FALSE)
                has_comments <- n_comments > 0L
            } else {
                has_comments <- logical(n_strings)
            }
            if (any_comments) {
                seq_comments <- numeric(n_strings)
                seq_comments[has_comments] <- seq_len(sum(has_comments))
                tmp_no_com <- no_com[has_comments]
                comment_idx <- lapply(tmp_no_com, `[[`, 3L)
                comments <- lapply(tmp_no_com, `[[`, 4L)
                comment_loc_remain <- lapply(tmp_no_com, `[[`, 5L)
                tmp_no_com <- NULL
                visit_comment <- rep.int(list(logical(0)), n_strings)
                visit_comment[has_comments] <-
                    lapply(lapply(comments, nchar), `>=`, 3L)
                if (any(has_urls)) {
                    has_urlcom <- has_comments | has_urls
                    orig_idx <- vector(mode = "list",
                                       length = sum(has_urlcom))
                    only_url <- !has_comments & has_urls
                    only_com <- has_comments & !has_urls
                    has_both <- has_comments & has_urls
                    orig_idx[only_url[has_urlcom]] <- url_idx[only_url]
                    orig_idx[only_com[has_urlcom]] <-
                        comment_idx[only_com[has_comments]]
                    orig_idx[has_both[has_urlcom]] <-
                        mapply(`[`, url_idx[has_both],
                               comment_idx[has_both[has_comments]],
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                    idx5 <- has_urls[has_comments]
                    comment_loc_remain[idx5] <-
                        mapply(`[`, url_idx[has_both],
                               comment_loc_remain[idx5],
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)
                } else {
                    has_urlcom <- has_comments
                    orig_idx <- comment_idx
                }
            } else {
                if (any(has_urls)) {
                    has_urlcom <- has_urls
                    orig_idx <- url_idx[has_urls]
                } else {
                    has_urlcom <- has_comments
                }
            }
            seq_urlcom <- numeric(n_idx)
            seq_urlcom[has_urlcom] <- seq_len(sum(has_urlcom))
            ## For each string that has addresses
            for (k in seq_len(n_idx)) {
                this_idx <- idx_addr[k]
                this_com <- has_comments[this_idx]
                this_urlcom <- has_urlcom[this_idx]
                orig_str <- remain[this_idx]
                nc_orig <- nchar(orig_str)
                this_str <- strings2[this_idx]
                addr_loc <- reg_addr[[this_idx]]
                addr_len <- match_len[[this_idx]]
                addr_next <- addr_loc + addr_len
                addr_last <- addr_next - 1
                nc <- nchar(this_str)
                remain_start <- c(1, addr_next)
                remain_end <- c(addr_loc - 1, nc)
                if (this_urlcom) {
                    o_idx <- orig_idx[[seq_urlcom[this_idx]]]
                    if (remain_end[1L] < 1) {
                        if (o_idx[1L] != 1) {
                            remain_start[1L] <- 0
                        } else {
                            remain_start <- remain_start[-1L]
                            remain_end <- remain_end[-1L]
                        }
                    }
                    n_remain <- length(remain_start)
                    if (remain_start[n_remain] > nc) {
                        if (o_idx[length(o_idx)] != nc_orig) {
                            remain_end[n_remain] <- nc + 1
                        } else {
                            remain_start <- remain_start[-n_remain]
                            remain_end <- remain_end[-n_remain]
                        }
                    }
                }
                tmp <- remain_end >= remain_start
                remain_start <- remain_start[tmp]
                remain_end <- remain_end[tmp]
                tmp <- NULL
                n_remain <- length(remain_start)
                ## Drop addresses where the domain part does not pass
                ## inspection with is_host: too many characters (RFC
                ## 2181), illegal literal address, ...
                h_loc <- host_loc[[this_idx]]
                host_part <- gsub("[[:space:]]+", "",
                                  substring(this_str, h_loc, addr_last),
                                  perl = TRUE)
                h_good <- is.infinite(is_host(host_part))
                if (any(h_good)) {
                    addr_loc <- addr_loc[h_good]
                    addr_len <- addr_len[h_good]
                    addr_last <- addr_last[h_good]
                    host_part <- host_part[h_good]
                    loc_len <- local_len[[this_idx]][h_good]
                    h_loc <- h_loc[h_good]
                    addr <- substring(this_str, addr_loc, addr_last)
                } else {
                    addr <- NULL
                }
                ## Drop prefixes which look like LaTeX macros
                if (length(addr) > 0L) {
                    lbrace <- regexpr("{", addr, fixed = TRUE)
                    test_latex <- which(lbrace != -1L)
                    if (length(test_latex) > 0L) {
                        loc_tmp <- addr_loc[test_latex] - 1
                        sub_m1 <- substring(this_str, loc_tmp,
                                            loc_tmp + lbrace[test_latex])
                        cut_latex <-
                            test_latex[grepl(paste0("^", k_latex, "$"),
                                             sub_m1, perl = TRUE)]
                        if (length(cut_latex) > 0L) {
                            lbrace_tmp <- lbrace[cut_latex]
                            addr_loc[cut_latex] <-
                                addr_loc[cut_latex] + lbrace_tmp
                            loc_len[cut_latex] <-
                                loc_len[cut_latex] - lbrace_tmp
                            addr[cut_latex] <-
                                substr(addr[cut_latex], 1 + lbrace_tmp,
                                       addr_len[cut_latex])
                            addr_len[cut_latex] <-
                                addr_len[cut_latex] - lbrace_tmp
                        }
                    }
                }
                ## Drop addresses where comments appear in a forbidden
                ## position
                if (length(addr) > 0L && this_com) {
                    cloc <- comment_loc[[this_idx]]
                    n_cloc <- length(cloc)
                    in_addr <- rep.int(NA_real_, nc)
                    pos_addr <- in_addr
                    for (l in seq_along(addr_loc)) {
                        addr_seq <- seq.int(from = addr_loc[l], by = 1,
                                            length.out = addr_len[l] - 1)
                        in_addr[addr_seq] <- l
                        pos_addr[addr_seq] <- seq_along(addr_seq)
                    }
                    check_cloc <- cloc >= 1L & cloc < nc
                    cloc_addr <- rep.int(NA_real_, n_cloc)
                    cloc_addr[check_cloc] <- in_addr[cloc[check_cloc]]
                    keep_cloc <- which(!is.na(cloc_addr))
                    cloc2 <- pos_addr[cloc[keep_cloc]]
                    cloc_addr <- cloc_addr[keep_cloc]
                    keep_addr <- rep.int(TRUE, length(addr))
                    for (l in which(!duplicated(cloc2))) {
                        this_addr <- addr[cloc_addr[l]]
                        before <- substr(this_addr, 1L, cloc2[l])
                        after <- substr(this_addr,
                                        cloc2[l] + 1, nchar(this_addr))
                        if (comment_is_bad(before, after)) {
                            keep_addr[cloc_addr[l]] <- FALSE
                        }
                    }
                    keep_addr <- which(keep_addr)
                    dont_visit <- keep_cloc[cloc_addr %in% keep_addr]
                    visit_comment[[this_idx]][dont_visit] <- FALSE
                    host_part <- host_part[keep_addr]
                    loc_len <- loc_len[keep_addr]
                    h_loc <- h_loc[keep_addr]
                    addr <- addr[keep_addr]
                }
                if (length(addr) > 0L) {
                    loc_part <- gsub("(?<!\\\\)[[:space:]]+", " ",
                                     substr(addr, 1L, loc_len), perl=TRUE)
                    ## Avoid some false positives in deobfuscated
                    ## addresses by requiring at least one letter in
                    ## both local and host part, no punctuation except
                    ## "." and "-", lengths at least 2 + 5.
                    if (deobfuscate) {
                        loc_aft <- substr(addr, loc_len + 1, nchar(addr))
                        deobfu <- grep("^[[:space:]]*+@", loc_aft,
                                       perl = TRUE, invert = TRUE)
                    }
                    if (deobfuscate && length(deobfu) > 0L) {
                        loc_deobfu <- loc_part[deobfu]
                        host_deobfu <- host_part[deobfu]
                        drop <- !grepl(k_nopunct, loc_deobfu, perl = TRUE)
                        drop <- drop |
                            !grepl(k_nopunct, host_deobfu, perl = TRUE)
                        drop <- drop |
                            !grepl(k_letter, loc_deobfu, perl = TRUE)
                        drop <- drop |
                            !grepl(k_letter, host_deobfu, perl = TRUE)
                        drop <- drop | nchar(loc_deobfu) < 2
                        drop <- drop | nchar(host_deobfu) < 5
                        if (any(drop)) {
                            drop <- deobfu[drop]
                            loc_part <- loc_part[-drop]
                            host_part <- host_part[-drop]
                            addr <- addr[-drop]
                            loc_len <- loc_len[-drop]
                            h_loc <- h_loc[-drop]
                        }
                    }
                    ## Avoid false positives: require letter or no spaces
                    if (length(addr) > 0L) {
                        drop_l <-
                            !grepl(k_letter, loc_part, perl = TRUE)
                        if (any(drop_l)) {
                            len_p <- loc_len[drop_l] + 1
                            drop_l[drop_l] <-
                                grepl("[[:space:]]",
                                      substr(addr[drop_l],
                                             len_p, len_p), perl = TRUE)
                        }
                        drop_r <-
                            !grepl(k_letter, host_part, perl = TRUE)
                        if (any(drop_r)) {
                            host_m <- h_loc[drop_r] - 1
                            drop_r[drop_r] <-
                                grepl("[[:space:]]",
                                      substring(this_str,
                                                host_m, host_m),
                                      perl = TRUE)
                        }
                        drop <- which(drop_l | drop_r)
                        if (length(drop) > 0L) {
                            loc_part <- loc_part[-drop]
                            host_part <- host_part[-drop]
                            addr <- addr[-drop]
                        }
                    }
                }
                if (length(addr) > 0L) {
                    addr <- paste0(loc_part, "@", host_part)
                    if (single_email) {
                        addr <- addr[1L]
                    }
                    eml[[this_idx]] <- addr
                }
                if (n_remain == 0L) {
                    remain[this_idx] <- ""
                    next
                }
                ## Remove addresses from string. Does not apply to
                ## addresses which are part of a URL. Does apply to
                ## malformed email addresses.
                if (this_urlcom) {
                    remain_start <-
                        restore_parts(o_idx, remain_start, nc_orig, TRUE)
                    remain_end <-
                        restore_parts(o_idx, remain_end, nc_orig, FALSE)
                    if (this_com) {
                        rem_loc <- rep.int(NA_real_, nc_orig)
                        remain_pos <- 0
                        for (l in seq_along(remain_start)) {
                            this_start <- remain_start[l]
                            this_end <- remain_end[l]
                            this_n <- this_end - this_start + 1L
                            rem_loc[this_start:this_end] <-
                                seq.int(remain_pos + 1,
                                        remain_pos + this_n)
                            remain_pos <- remain_pos + this_n + 1
                        }
                        seq_com <- seq_comments[this_idx]
                        comment_loc_remain[[seq_com]] <-
                            rem_loc[comment_loc_remain[[seq_com]]] +
                            (remain_start[1L] != 1L)
                    }
                }
                subs_remain <-
                    substring(orig_str, remain_start, remain_end)
                if (remain_start[1L] != 1L) {
                    subs_remain <- c("", subs_remain)
                }
                if (remain_end[n_remain] != nc_orig) {
                    subs_remain <- c(subs_remain, "")
                }
                remain[this_idx] <- paste0(subs_remain, collapse = " ")
            }
            stack_n[["eml"]] <- eml
            stack_n[["remain"]] <- remain
            work_stack[[n_stack]] <- stack_n
            if (any_comments) {
                ## If a "comment" (text in parentheses) is not inside
                ## an email address, look inside the parentheses (go
                ## for another round of the loop).
                do_visit <-
                    vapply(visit_comment, any, FALSE, USE.NAMES = FALSE)
                if (single_email) {
                    ## Prefer email address which is not in parentheses
                    do_visit <- do_visit & !nzchar(eml)
                }
                which_visit <- which(do_visit)
                n_which <- length(which_visit)
                if (n_which > 0L) {
                    visit_strings <- character(0)
                    visit_loc <- numeric(0)
                    new_id <- numeric(0)
                    for (k in seq_len(n_which)) {
                        this_idx <- which_visit[k]
                        seq_com <- seq_comments[this_idx]
                        visit_flag <- visit_comment[[this_idx]]
                        k_strings <- comments[[seq_com]][visit_flag]
                        new_id <- c(new_id,
                                    rep.int(visit_id[this_idx],
                                            length(k_strings)))
                        visit_strings <- c(visit_strings, k_strings)
                        visit_loc <-
                            c(visit_loc,
                              comment_loc_remain[[seq_com]][visit_flag])
                    }
                    remain <- visit_strings
                    has_urls <- logical(length(visit_strings))
                    visit_id <- new_id
                    visiting <- unique(visit_id)
                    visit_start <- which(diff(c(0, visit_id)) != 0)
                    n_visit <- diff(c(visit_start, length(visit_loc) + 1))
                    work_stack[[n_stack + 1]] <-
                        list(strings = visit_strings, visit = visiting,
                             visit.start = visit_start,
                             n.visit = n_visit, visit.loc = visit_loc)
                }
            }
        } # end of loop that goes inside parenthesis
        ## Combine the results
        while (n_stack > 1) {
            stack_n <- work_stack[[n_stack]]
            stack_m1 <- work_stack[[n_stack - 1]]
            tmp_eml <- eml
            tmp_remain <- remain
            eml <- stack_m1[["eml"]]
            remain <- stack_m1[["remain"]]
            visit_strings <- stack_n[["strings"]]
            visiting <- match(stack_n[["visit"]], stack_m1[["visit"]])
            visit_start <- stack_n[["visit.start"]]
            n_visit <- stack_n[["n.visit"]]
            tmp_loc <- stack_n[["visit.loc"]]
            for (k in seq_along(visiting)) {
                visit_k <- visiting[k]
                pick_idx <- seq.int(from = visit_start[k], by = 1,
                                    length.out = n_visit[k])
                pick_eml <- tmp_eml[pick_idx]
                pick_remain <- tmp_remain[pick_idx]
                k_strings <- visit_strings[pick_idx]
                changed <- which(pick_remain != k_strings)
                if (length(changed) == 0L) {
                    next
                }
                if (single_email) {
                    eml[[visit_k]] <- pick_eml[changed[1L]]
                } else {
                    eml[[visit_k]] <-
                        c(eml[[visit_k]],
                          unlist(pick_eml[changed],
                                 recursive = FALSE, use.names = FALSE))
                }
                visit_loc <- tmp_loc[pick_idx]
                visit_end <- visit_loc - 1 + nchar(k_strings)
                other_loc <- c(1, visit_end + 1)
                other_end <- c(visit_loc - 1, nchar(remain[visit_k]))
                tmp <- other_end >= other_loc
                other_loc <- other_loc[tmp]
                other_end <- other_end[tmp]
                part_order <- order(c(other_loc, visit_loc))
                pick_remain[!nzchar(pick_remain)] <- " "
                remain[visit_k] <-
                    paste0(c(substring(remain[visit_k],
                                       other_loc, other_end),
                             pick_remain)[part_order],
                           collapse = "")
            }
            n_stack <- n_stack - 1
        }
        list(eml, remain)
    } # end of pick_email
    pick_start_email <- function(string, allow_comments = FALSE,
                                 deobfuscate = FALSE) {
        pe <- pick_email(string, single_email = TRUE, url_check = FALSE,
                         allow_comments = allow_comments,
                         deobfuscate = deobfuscate)[[1L]]
        nc_pe <- nchar(pe)
        permissive <- allow_comments || deobfuscate
        if (nc_pe == 0L ||
            (!permissive && substr(string, 1L, nc_pe) != pe) ||
            (permissive &&
             pick_email(substr(string, 2L, nchar(string)),
                        single_email = TRUE, url_check = FALSE,
                        allow_comments = allow_comments,
                        deobfuscate = deobfuscate)[[1L]] == pe)) {
            ""
        } else {
            pe
        }
    }
    is_plain_email <- function(strings) {
        pe <- pick_email(strings, single_email = FALSE, url_check = FALSE,
                         deobfuscate = FALSE, allow_comments = FALSE)
        eml <- pe[[1L]]
        remain <- pe[[2L]]
        result <- !nzchar(remain)
        result[result] <- lengths(eml[result], use.names = FALSE) == 1L
        result[result] <- unlist(eml[result], recursive = FALSE,
                                 use.names = FALSE) == strings[result]
        result
    }
    check_addresses <- function(string) {
        questions <- gregexpr("?", string, fixed = TRUE)[[1L]]
        if (questions[1L] != -1L) {
            if (length(questions) == 1L) {
                str1 <- substr(string, 1, questions - 1)
                str2 <- substr(string, questions + 1, nchar(string))
            } else {
                str1 <- substr(string, 1, questions[1L] - 1)
                str2 <- substr(string, questions[1L] + 1,
                               questions[2L] - 1)
            }
            k_hfield <- "[^=&]*+=[^[:space:]=&]*+"
            str2 <- sub(paste0("^(", k_hfield,
                               "(?:&", k_hfield, ")*+)?+.*"),
                        "\\1", str2, perl = TRUE)
            add_hfields <- nzchar(str2)
        } else {
            str1 <- string
            add_hfields <- FALSE
        }
        if (!add_hfields) {
            str1 <- sub("[[:space:]].*", "", str1, perl = TRUE)
        }
        nc <- nchar(str1)
        count_comma <- rep.int(TRUE, nc)
        qs <- gregexpr(paste0("\"", k_qcont, "*+\""), str1,
                       perl = TRUE)[[1L]]
        brackets <- gregexpr("\\[[^][]++]", str1, perl = TRUE)[[1L]]
        for (check in list(qs, brackets)) {
            if (check[1L] != -1L) {
                check_len <- attr(check, "match.length")
                check_last <- check - 1 + check_len
                for (k in seq_along(check)) {
                    count_comma[check[k]:check_last[k]] <- FALSE
                }
            }
        }
        is_comma <- logical(nc)
        commas <- gregexpr(",", str1, fixed = TRUE)[[1L]]
        if (commas[1L] != -1L) {
            is_comma[commas] <- TRUE
        }
        bare_comma <- which(is_comma & count_comma)
        if (length(bare_comma) == 0L) {
            parts <- str1
        } else {
            part_s <- c(1, bare_comma + 1)
            part_e <- c(bare_comma - 1, nc)
            parts <- substring(str1, part_s, part_e)
        }
        part_good <- is_plain_email(parts)
        first_bad <- which(!part_good)[1L]
        if (is.na(first_bad)) {
            if (add_hfields) {
                paste0(str1, "?", str2)
            } else {
                str1
            }
        } else {
            cut_part <- pick_start_email(parts[first_bad])
            if (nzchar(cut_part)) {
                paste0(c(parts[seq_len(first_bad - 1L)], cut_part),
                       collapse = ",")
            } else {
                paste0(parts[seq_len(first_bad - 1L)], collapse = ",")
            }
        }
    } # end of check_addresses
    ## pick_one_url: For each string, extracts URL if found. If that
    ## fails and 'only_url' is FALSE, extracts a "word" or other
    ## substring (possibly empty). When 'remaining' is TRUE, also
    ## returns the parts before and after the URL. Arguments
    ## 'cut_comma_email' and 'split_userinfo' are only passed through
    ## to split_and_check(). The treatment of trailing commas depends
    ## on the value of 'ignore_commas'. Leading punctuation is kept if
    ## 'keep_punct' is TRUE. "URLs" inside literal IP addresses are
    ## not picked up if 'only_url' is TRUE'; 'allow_space' is passed
    ## to find_ip_literal() and if TRUE, literal IP addresses may have
    ## spaces when used for this purpose .
    pick_one_url <- function(strings, remaining=FALSE, only_url=FALSE,
                             split_userinfo=FALSE, keep_punct=FALSE,
                             ignore_commas=TRUE, cut_comma_email=FALSE,
                             allow_space=FALSE, cache_str=NULL,
                             cache_idx=NULL, cache_ip=NULL) {
        n_strings <- length(strings)
        has_url <- logical(n_strings)
        nc_strings <- nchar(strings)
        pre <- character(n_strings)
        post <- pre
        backup_pre <- pre
        backup_post <- pre
        post_drop <- numeric(n_strings)
        if (is.null(cache_str)) {
            work <- seq_len(n_strings)
            str2 <- pre
            str_idx <- vector(mode = "list", length = n_strings)
            keep_work <- rep.int(TRUE, n_strings)
            for (k in seq_along(work)) {
                this_work <- work[k]
                string_k <- strings[this_work]
                nc_k <- nc_strings[this_work]
                drop_chars <-
                    gregexpr(k_replace_nsp, string_k, perl = TRUE)[[1L]]
                if (drop_chars[1L] == -1L) {
                    str2[this_work] <- string_k
                    str_idx[[this_work]] <- seq_len(nc_k)
                } else {
                    keep_flag <- rep.int(TRUE, nc_k)
                    drop_len <- attr(drop_chars, "match.length")
                    for (l in seq_along(drop_chars)) {
                        keep_flag[seq.int(from = drop_chars[l], by = 1,
                                          length.out = drop_len[l])] <-
                                              FALSE
                    }
                    if (any(keep_flag)) {
                        keep_diff <- diff(c(0L, keep_flag, 0L))
                        keep_loc <- which(keep_diff == 1L)
                        keep_last <- which(keep_diff == -1L) - 1L
                        keep_len <- keep_last - keep_loc + 1L
                        n_keep <- length(keep_loc)
                        str2[this_work] <-
                            paste0(substring(string_k,
                                             keep_loc, keep_last),
                                   collapse = "\b")
                        this_idx <- rep.int(NA_real_,
                                            sum(keep_len) - 1 + n_keep)
                        loc <- 1
                        for (l in seq_len(n_keep)) {
                            this_len <- keep_len[l]
                            this_idx[seq.int(from = loc, by = 1,
                                             length.out = this_len)] <-
                                                 keep_loc[l]:keep_last[l]
                            loc <- loc + 1 + this_len
                        }
                        str_idx[[this_work]] <- this_idx
                        post_drop[this_work] <- nc_k - keep_last[l]
                    } else {
                        pre[this_work] <- string_k
                        str2[this_work] <- ""
                        str_idx[[this_work]] <- numeric(0)
                        keep_work[k] <- FALSE
                    }
                }
            }
            work <- work[keep_work]
            n_work <- length(work)
        } else {
            str2 <- cache_str
            str_idx <- cache_idx
            nz_str2 <- nzchar(str2)
            work <- which(nz_str2)
            n_work <- length(work)
            if (n_work < n_strings) {
                z_idx <- which(!nz_str2)
                pre[z_idx] <- strings[z_idx]
            }
        }
        if (is.null(cache_ip)) {
            has_ip_lit <- logical(n_strings)
            in_ip_lit <- vector(mode = "list", length = n_strings)
            for (k in seq_len(n_work)) {
                work_k <- work[k]
                str_k <- str2[work_k]
                lits <- find_ip_literal(str_k, allow_space)
                if (is.null(lits)) {
                    next
                }
                has_ip_lit[work_k] <- TRUE
                in_this <- logical(nchar(str_k))
                lit_loc <- lits[[1L]]
                lit_last <- lits[[2L]]
                n_lits <- length(lit_loc)
                for (l in seq_len(n_lits)) {
                    in_this[lit_loc[l]:lit_last[l]] <- TRUE
                }
                in_ip_lit[[work_k]] <- in_this
            }
        } else {
            in_ip_lit <- cache_ip
            has_ip_lit <- vapply(in_ip_lit, is.logical, FALSE,
                                 USE.NAMES = FALSE)
        }
        return_idx <- remaining && only_url
        if (return_idx) {
            str_out <- str2
            idx_out <- str_idx
            ip_out <- in_ip_lit
        }
        ## 1. At the beginning, with possible white space and / or
        ##    "URL:" prefix(es) (case insensitive).
        ## 2. With at least one "URL:" prefix (case insensitive).
        ## 3. With possible preceding punctuation which is kept for
        ##    matching with possible trailing punctuation (keep_punct)
        ##    or without punctuation (!keep_punct).
        k_char <- "[^[:space:][:cntrl:]]"
        k_pats <-
            c(paste0("^[[:space:]]*+(?:[Uu][Rr][Ll]:[[:space:]]*+)*+((",
                     k_scheme, "+):", k_char, "*+[[:space:]]?)"),
              paste0("(?<![", k_ascii52,
                     "])(?:[Uu][Rr][Ll]:[[:space:]]*+)++((",
                     k_scheme, "+):", k_char, "*+[[:space:]]?)"))
        if (keep_punct) {
            k_pats <- c(k_pats, paste0("[[:punct:]]*", k_sch_colon_noposs,
                                       k_char, "*[[:space:]]?"))
            k_perl <- c(TRUE, TRUE, FALSE)
        } else {
            k_pats <- c(k_pats, paste0("((", k_scheme, "+):", k_char,
                                       "*+[[:space:]]?)"))
            k_perl <- c(TRUE, TRUE, TRUE)
        }
        k_alnum <- paste0(k_char, "*[[:alnum:]]", k_char, "*+[[:space:]]?")
        k_nsp <- paste0(k_char, "++[[:space:]]?")
        skip2 <- numeric(n_strings)
        discarded <- character(0)
        while (n_work > 0L) {
            work0 <- work
            for (k in seq_along(k_pats)) {
                str3 <- str2
                skip3 <- skip2
                keep_work <- rep.int(TRUE, n_work)
                local_work <- rep.int(TRUE, n_work)
                while (any(local_work)) {
                    work1 <- which(local_work)
                    work2 <- work[work1]
                    pat <- regexpr(k_pats[k], str3[work2], perl=k_perl[k])
                    has_pat <- which(pat != -1L)
                    if (length(has_pat) == 0L) {
                        break
                    }
                    if (k_perl[k]) {
                        cap_s <-
                            attr(pat,
                                 "capture.start")[has_pat, , drop=FALSE]
                        cap_l <-
                            attr(pat,
                                 "capture.length")[has_pat, , drop=FALSE]
                        sub_loc <- cap_s[, 1L]
                        sub_len <- cap_l[, 1L]
                        sch_loc <- cap_s[, 2L]
                        sch_len <- cap_l[, 2L]
                    } else {
                        sub_loc <- pat[has_pat]
                        sub_len <- attr(pat, "match.length")[has_pat]
                        sch_loc <- regexpr(paste0(k_scheme, "+(?=:)"),
                                           str3[work2[has_pat]],
                                           perl = TRUE)
                        sch_len <- attr(sch_loc, "match.length")
                    }
                    w_pat <- work2[has_pat]
                    sub_last <- sub_loc - 1 + sub_len
                    the_sub <- substr(str3[w_pat], sub_loc, sub_last)
                    ## Avoid repeated work (previously discarded items)
                    discard <- which(the_sub %in% discarded)
                    if (length(discard) > 0L) {
                        sch_loc <- sch_loc[-discard]
                        w_disc <- w_pat[discard]
                        add_skip <- sub_last[discard]
                        skip3[w_disc] <- skip3[w_disc] + add_skip
                        str_disc <- str3[w_disc]
                        str3[w_disc] <- substr(str_disc, add_skip + 1,
                                               nchar(str_disc))
                        if (length(sch_loc) == 0L) {
                            next
                        }
                        sch_len <- sch_len[-discard]
                        w_pat <- w_pat[-discard]
                        sub_loc <- sub_loc[-discard]
                        sub_last <- sub_last[-discard]
                        the_sub <- the_sub[-discard]
                        has_pat <- has_pat[-discard]
                    }
                    ## Skip items without a URL scheme
                    sch_last <- sch_loc - 1 + sch_len
                    sch <- tolower(substr(str3[w_pat],
                                          sch_loc, sch_last))
                    sch_url <- which(sch == "url")
                    if (length(sch_url) > 0L) {
                        sch <- sch[-sch_url]
                        w_nosch <- w_pat[sch_url]
                        add_skip <- sch_last[sch_url] + 1
                        fix_add <- sch_len[sch_url] == 0
                        add_skip[fix_add] <- sub_loc[sch_url[fix_add]] - 1
                        skip3[w_nosch] <- skip3[w_nosch] + add_skip
                        str_nosch <- str3[w_nosch]
                        str3[w_nosch] <- substr(str_nosch, add_skip + 1,
                                                nchar(str_nosch))
                        discarded <- c(discarded,
                                       unique(the_sub[sch_url]))
                        if (length(sch) == 0L) {
                            next
                        }
                        sch_last <- sch_last[-sch_url]
                        w_pat <- w_pat[-sch_url]
                        sub_loc <- sub_loc[-sch_url]
                        sub_last <- sub_last[-sch_url]
                        the_sub <- the_sub[-sch_url]
                        has_pat <- has_pat[-sch_url]
                    }
                    ## Skip items inside an IP literal address
                    ip_lit <- logical(length(has_pat))
                    for (l in which(has_ip_lit[w_pat])) {
                        w_l <- w_pat[l]
                        s_loc <- skip3[w_l] + sub_loc[l] +
                            attr(regexpr("^[[:punct:]]*", the_sub[l]),
                                 "match.length")
                        if (in_ip_lit[[w_l]][s_loc]) {
                            ip_lit[l] <- TRUE
                        }
                    }
                    ip_lit <- which(ip_lit)
                    if (length(ip_lit) > 0L) {
                        sch <- sch[-ip_lit]
                        w_lit <- w_pat[ip_lit]
                        add_skip <- sub_last[ip_lit]
                        skip3[w_lit] <- skip3[w_lit] + add_skip
                        str_lit <- str3[w_lit]
                        str3[w_lit] <- substr(str_lit, add_skip + 1,
                                              nchar(str_lit))
                        if (length(sch) == 0L) {
                            next
                        }
                        sch_last <- sch_last[-ip_lit]
                        w_pat <- w_pat[-ip_lit]
                        sub_loc <- sub_loc[-ip_lit]
                        sub_last <- sub_last[-ip_lit]
                        the_sub <- the_sub[-ip_lit]
                        has_pat <- has_pat[-ip_lit]
                    }
                    do_again <- logical(length(w_pat))
                    idx_pat <- str_idx[w_pat]
                    skip_w <- skip3[w_pat]
                    nc_w <- nc_strings[w_pat]
                    pre_loc <- mapply(restore_parts, idx_pat,
                                      sub_loc - 1 + skip_w, nc_w, FALSE,
                                      USE.NAMES = FALSE)
                    post_loc <- mapply(restore_parts, idx_pat,
                                       sub_last + 1 + skip_w, nc_w, TRUE,
                                       USE.NAMES = FALSE)
                    the_pre <- substr(strings[w_pat], 1L, pre_loc)
                    the_post <- substr(strings[w_pat], post_loc, nc_w)
                    ## Cut emails: check validity of individual
                    ## addresses (parts separated by commas), allow
                    ## optional hfields part at the end of the string
                    has_mailto <- which(sch %in% mail_alias)
                    if (length(has_mailto) > 0L) {
                        mailto_strings <- str3[w_pat[has_mailto]]
                        colon_loc <- sch_last[has_mailto] + 1
                        address0 <-
                            sub("^[[:space:]]+", "",
                                substr(mailto_strings, colon_loc + 1,
                                       sub_last[has_mailto]), perl = TRUE)
                        address <- vapply(address0, check_addresses, "",
                                          USE.NAMES = FALSE)
                        nc_address <- nchar(address)
                        nz_addr <- nc_address > 0L
                        mailto_prefix <-
                            substr(mailto_strings, sub_loc[has_mailto],
                                   colon_loc)
                        the_sub[has_mailto[nz_addr]] <-
                            paste0(mailto_prefix[nz_addr],
                                   address[nz_addr])
                        z_addr <- !nz_addr
                        z_fix <- has_mailto[z_addr]
                        if (length(z_fix) > 0L) {
                            do_again[z_fix] <- TRUE
                            z_strings <- mailto_strings[z_addr]
                            idx3 <- w_pat[z_fix]
                            discarded <- c(discarded,
                                           unique(the_sub[z_fix]))
                            z_col <- colon_loc[z_addr]
                            str3[idx3] <- substr(z_strings, z_col + 1,
                                                 nchar(z_strings))
                            new_skip <- skip3[idx3] + z_col
                            skip3[idx3] <- new_skip
                            the_pre[z_fix] <-
                                paste0(the_pre[z_fix],
                                       mailto_prefix[z_addr])
                        }
                        nc0 <- nchar(address0)
                        shortened <- nz_addr & nc_address < nc0
                        idx_short <- has_mailto[shortened]
                        if (length(idx_short) > 0L) {
                            tail_s <- nc_address[shortened] + 1
                            tail_e <- nc0[shortened]
                            the_post[idx_short] <-
                                paste0(substr(address0[shortened],
                                              tail_s, tail_e),
                                       the_post[idx_short])
                        }
                    }
                    not_again <- !do_again
                    w_pat <- w_pat[not_again]
                    str2[w_pat] <- the_sub[not_again]
                    skip2[w_pat] <- skip3[w_pat] - 1 + sub_loc[not_again]
                    pre[w_pat] <- the_pre[not_again]
                    post[w_pat] <- the_post[not_again]
                    has_url[w_pat] <- TRUE
                    local_work <- logical(n_work)
                    tmp <- work1[has_pat]
                    local_work[tmp] <- do_again
                    keep_work[tmp] <- do_again
                }
                work <- work[keep_work]
                n_work <- length(work)
            }
            if (!only_url) {
                ## 4. k_alnum. For remaining strings, take first
                ##    sequence of URL-legal characters with at least
                ##    one alnum character.
                ## 5. k_nsp. If step 4 fails, take first sequence of
                ##    URL-legal characters (nsp means no spaces). Empty
                ##    result possible.
                for (re in c(k_alnum, k_nsp)) {
                    re_match <- regexpr(re, str2[work], perl = TRUE)
                    has_re <- re_match != -1L
                    work_re <- work[has_re]
                    if (length(work_re) == 0L) {
                        next
                    }
                    re_len <- attr(re_match, "match.length")[has_re]
                    re_match <- re_match[has_re]
                    re_last <- re_match - 1 + re_len
                    str2[work_re] <-
                        substr(str2[work_re], re_match, re_last)
                    idx_re <- str_idx[work_re]
                    nc_re <- nc_strings[work_re]
                    skip_re <- skip2[work_re]
                    skip2[work_re] <- skip_re - 1 + re_match
                    pre_re <- mapply(restore_parts, idx_re,
                                     re_match - 1 + skip_re,
                                     nc_re, FALSE, USE.NAMES = FALSE)
                    post_re <- mapply(restore_parts, idx_re,
                                      re_last + 1 + skip_re,
                                      nc_re, TRUE, USE.NAMES = FALSE)
                    pre[work_re] <- substr(strings[work_re], 1L, pre_re)
                    post[work_re] <- substr(strings[work_re],
                                            post_re, nc_re)
                    work <- work[!has_re]
                    n_work <- length(work)
                }
                if (n_work > 0L) {
                    str2[work] <- ""
                    pre[work] <- strings[work]
                }
            }
            n_work <- 0L
            ## Postprocessing
            if (ignore_commas) {
                clip_pat <- "[[:space:]]$"
            } else {
                clip_pat <- ",?+[[:space:]]$"
            }
            endspace <- regexpr(clip_pat, str2[work0], perl = TRUE)
            has_endspace <- endspace != -1L
            if (any(has_endspace)) {
                work_esp <- work0[has_endspace]
                idx_esp <- str_idx[work_esp]
                nc_esp <- nc_strings[work_esp]
                skip_esp <- skip2[work_esp]
                esp <- endspace[has_endspace]
                post_esp <- mapply(restore_parts, idx_esp, esp + skip_esp,
                                   nc_esp, TRUE, USE.NAMES = FALSE)
                post[work_esp] <- substr(strings[work_esp],
                                         post_esp, nc_esp)
                str2[work_esp] <- substr(str2[work_esp], 1L, esp - 1L)
            }
            idx <- work0[nzchar(str2[work0])]
            if (length(idx) > 0L) {
                str2_idx <- str2[idx]
                nc_in <- nchar(str2_idx)
                nc_punct <- attr(regexpr("^[[:punct:]]*", str2_idx),
                                 "match.length")
                puncts <- substr(str2_idx, 1L, nc_punct)
                tmp <- lapply(substr(str2_idx, nc_punct + 1, nc_in),
                              split_and_check, only_first = TRUE,
                              split_userinfo = split_userinfo,
                              remaining = TRUE,
                              cut_comma_email = cut_comma_email)
                ## Each component of 'result' is always a (possibly
                ## empty) substring at the beginning of the
                ## corresponding input string
                result <- vapply(tmp, `[[`, "", 1L, USE.NAMES = FALSE)
                ## 'remain' is always a strict substring at the end of
                ## the input. Nothing between 'result' and 'remain' is
                ## lost when 'result' is a URL.
                remain <-  vapply(tmp, `[[`, "", 2L, USE.NAMES = FALSE)
                tmp <- NULL
                nc_result <- nchar(result)
                diff_nc <- nc_in - nc_result - nc_punct
                is_good <- nc_result > 0L
                idx_good <- idx[is_good]
                str2[idx_good] <- paste0(puncts[is_good], result[is_good])
                less_out <- diff_nc > 0L
                idx_good_less <- idx_good[less_out[is_good]]
                if (length(idx_good_less) > 0L) {
                    good_less <- is_good & less_out
                    post[idx_good_less] <-
                        paste0(remain[good_less], post[idx_good_less])
                }
                if (!all(is_good)) {
                    is_bad <- !is_good
                    idx_bad <- idx[is_bad]
                    no_backup <- idx_bad[!nzchar(backup_pre[idx_bad])]
                    backup_pre[no_backup] <- paste0(pre[no_backup],
                                                    str2[no_backup])
                    backup_post[no_backup] <- post[no_backup]
                    str2[idx_bad] <- paste0(remain[is_bad], post[idx_bad])
                    add_skip <- diff_nc[is_bad] - nchar(remain[is_bad])
                    skip2[idx_bad] <- skip2[idx_bad] + add_skip
                    more_work <- nchar(str2[idx_bad]) > post_drop[idx_bad]
                    ## Some strings may need another round
                    work <- idx_bad[more_work]
                    n_work <- length(work)
                    no_work <- idx_bad[!more_work]
                    str2[no_work] <- ""
                    pre[no_work] <- backup_pre[no_work]
                    post[no_work] <- backup_post[no_work]
                    has_url[idx_bad] <- FALSE
                }
            }
        }
        no_url <- which(!has_url)
        if (only_url) {
            str2[no_url] <- ""
            pre[no_url] <- strings[no_url]
            post[no_url] <- ""
        } else if (length(no_url) > 0L) {
            sch_loc <- regexpr(paste0(k_scheme, "+(?=:)"),
                               str2[no_url], perl = TRUE)
            sch_found <- sch_loc != -1L
            test_mailto <- no_url[sch_found]
            if (length(test_mailto) > 0L) {
                sch_len <- attr(sch_loc, "match.length")[sch_found]
                sch_loc <- sch_loc[sch_found]
                sch_last <- sch_loc - 1 + sch_len
                sch <- tolower(substr(str2[test_mailto],
                                      sch_loc, sch_last))
                idx_mailto <- test_mailto[sch %in% mail_alias]
                if (length(idx_mailto) > 0L) {
                    str2[idx_mailto] <- ""
                    if (remaining) {
                        pre[idx_mailto] <-
                            paste0(pre[idx_mailto], str2[idx_mailto])
                    }
                }
            }
        }
        if (return_idx) {
            list(str2, post, pre, str_out, idx_out, ip_out)
        } else if (remaining) {
            list(str2, post, pre)
        } else {
            list(str2, has_url)
        }
    } # end of pick_one_url
    ## pick_one_plus_url: Return all comma separated URLs (or other
    ## substrings) as one character vector. If possible, tries to view
    ## commas as part of the URL. If that fails, the function falls
    ## back to heuristic splitting for the rest of the string.
    pick_one_plus_url <- function(strings) {
        tmp <- pick_one_url(strings, remaining = TRUE)
        result <- tmp[[1L]]
        remain <- tmp[[2L]]
        tmp <- NULL
        remain <- remain[nzchar(remain)]
        if (length(remain) > 0L) {
            result <- c(result,
                        unlist(lapply(remain, split_and_check,
                                      split_userinfo = TRUE,
                                      cut_comma_email = TRUE),
                               recursive = FALSE, use.names = FALSE))
        }
        result[nzchar(result)]
    }
    remove_urls <- function(strings, schemes_allowed = character(0),
                            deobfuscate, skip_quoted_email = TRUE) {
        n_strings <- length(strings)
        do_skip <- FALSE
        if (skip_quoted_email) {
            nc_strings <- nchar(strings)
            qpat <- if (deobfuscate) {
                paste0(k_qs, "(?=[[:space:]]*+(?:[(@]|[aA][tT]))")
            } else {
                paste0(k_qs, "(?=[[:space:]]*+[(@])")
            }
            reg_quotes <- gregexpr(qpat, strings, perl = TRUE)
            has_quotes <- vapply(reg_quotes, `[`, -1L, 1L,
                                 USE.NAMES = FALSE) != -1L
            idx_quotes <- which(has_quotes)
            if (length(idx_quotes) > 0L) {
                do_skip <- TRUE
                quote_first <- vector(mode = "list", length = n_strings)
                quote_last <- quote_first
                last_qchar <- rep.int(NA_real_, n_strings)
                for (k in idx_quotes) {
                    this_first <- rep.int(NA_real_, nc_strings[k])
                    this_last <- this_first
                    this_loc <- reg_quotes[[k]]
                    this_len <- attr(this_loc, "match.length")
                    for (l in seq_along(this_loc)) {
                        tmp_seq <- seq.int(from = this_loc[l], by = 1,
                                           length.out = this_len[l])
                        this_first[tmp_seq] <- this_loc[l]
                        this_last[tmp_seq] <-
                            this_loc[l] - 1 + this_len[l]
                    }
                    quote_first[[k]] <- this_first
                    quote_last[[k]] <- this_last
                    last_qchar[k] <- this_last[tmp_seq[1L]]
                }
                tmp_seq <- NULL
            }
        }
        if (length(schemes_allowed) > 0L) {
            sch <- paste0(schemes_allowed, ":")
            nc_sch <- nchar(sch)
        } else {
            sch <- schemes_allowed
        }
        work <- seq_len(n_strings)
        str2 <- rep.int(list(character(0)), n_strings)
        str_idx <- rep.int(list(list()), n_strings)
        todo <- strings
        n_work <- n_strings
        pending <- character(n_work)
        skip <- numeric(n_work)
        cache_str <- NULL
        cache_idx <- NULL
        cache_ip <- NULL
        while (n_work > 0L) {
            pou <- pick_one_url(todo, only_url=TRUE, split_userinfo=TRUE,
                                remaining = TRUE, cut_comma_email = TRUE,
                                allow_space = TRUE, cache_str = cache_str,
                                cache_idx = cache_idx, cache_ip=cache_ip)
            url <- pou[[1L]]
            post <- pou[[2L]]
            pre <- pou[[3L]]
            cache_str <- pou[[4L]]
            cache_idx <- pou[[5L]]
            cache_ip <- pou[[6L]]
            pou <- NULL
            nz_url <- nzchar(url)
            nc_pre <- nchar(pre)
            nc_pending <- nchar(pending)
            if (do_skip && any(nz_url)) {
                quote_and_url <- which(nz_url & has_quotes)
                nc_url <- nchar(url)
                processed <- skip + nc_pending
                url_loc <- processed + nc_pre + 1
                for (k in quote_and_url) {
                    work_k <- work[k]
                    uloc <- url_loc[k]
                    ## Skip (URLs in) a quoted string if it is part of
                    ## an email address
                    sub_start <- quote_first[[work_k]][uloc]
                    if (is.na(sub_start)) {
                        next
                    }
                    sub_k <- substr(strings[work_k], sub_start,
                                    nc_strings[work_k])
                    pse <- pick_start_email(sub_k, allow_comments = TRUE,
                                            deobfuscate = deobfuscate)
                    if (!nzchar(pse)) {
                        next
                    }
                    ql <- quote_last[[work_k]][uloc]
                    has_quotes[k] <- last_qchar[work_k] > ql
                    pass <- ql - processed[k]
                    todo_k <- todo[k]
                    pending[k] <- paste0(pending[k],
                                         substr(todo_k, 1, pass))
                    nc_pending[k] <- nc_pending[k] + pass
                    pre[k] <- ""
                    nc_pre[k] <- 0L
                    url[k] <- ""
                    nc_url[k] <- 0L
                    nz_url[k] <- FALSE
                    post[k] <- substr(todo_k, pass + 1, nchar(todo_k))
                }
            }
            if (any(nz_url)) {
                if (!do_skip) {
                    nc_url <- nchar(url)
                }
                for (k in seq_along(sch)) {
                    sch_match <- substr(url, 1L, nc_sch[k]) == sch[k]
                    ## Skip URLs with any of the listed schemes
                    if (any(sch_match)) {
                        pending[sch_match] <-
                            paste0(pending[sch_match], pre[sch_match],
                                   url[sch_match])
                        nc_pending[sch_match] <- nc_pending[sch_match] +
                            nc_pre[sch_match] + nc_url[sch_match]
                        pre[sch_match] <- ""
                        nc_pre[sch_match] <- 0L
                        url[sch_match] <- ""
                        nc_url[sch_match] <- 0L
                        nz_url[sch_match] <- FALSE
                        if (!any(nz_url)) {
                            break
                        }
                    }
                }
            } else {
                nc_url <- numeric(n_work)
            }
            nz_pre <- nc_pre > 0L
            pending[nz_pre] <- paste0(pending[nz_pre], pre[nz_pre])
            nc_pending[nz_pre] <- nc_pending[nz_pre] + nc_pre[nz_pre]
            do_more <- nzchar(post)
            ## Flushing creates a gap in the output; see k_mail_sep below
            flushed <- nc_pending > 0L & (nz_url | !do_more)
            if (any(flushed)) {
                work_flush <- work[flushed]
                str2[work_flush] <-
                    mapply(c, str2[work_flush], pending[flushed],
                           SIMPLIFY = FALSE, USE.NAMES = FALSE)
                new_idx <-
                    lapply(mapply(`+`,
                                  lapply(nc_pending[flushed], seq_len),
                                  skip[flushed], SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE), list)
                str_idx[work_flush] <-
                    mapply(c, str_idx[work_flush], new_idx,
                           SIMPLIFY = FALSE, USE.NAMES = FALSE)
            }
            if (any(do_more)) {
                skip <- skip + nc_url +
                    ifelse(flushed, nc_pending, nc_pre)
                pending[flushed] <- ""
                work <- work[do_more]
                pending <- pending[do_more]
                skip <- skip[do_more]
                nc1 <- nchar(todo[do_more])
                todo <- post[do_more]
                nc2 <- nchar(todo)
                progress <- nc1 - nc2
                cache_str <- cache_str[do_more]
                cache_idx <- cache_idx[do_more]
                cache_ip <- cache_ip[do_more]
                n_work <- length(work)
                for (k in seq_len(n_work)) {
                    idx_k <- cache_idx[[k]]
                    progress_k <- progress[k]
                    keep_idx <- idx_k > progress_k
                    first_keep <- which.max(keep_idx)
                    if (length(first_keep) > 0L && keep_idx[first_keep]) {
                        last_keep <- length(idx_k)
                        cache_str[k] <-
                            substr(cache_str[k], first_keep, last_keep)
                        first_last <- first_keep:last_keep
                        cache_idx[[k]] <- idx_k[first_last] - progress_k
                        ip_k <- cache_ip[[k]]
                        if (!is.null(ip_k)) {
                            ip_k <- ip_k[first_last]
                            if (any(ip_k)) {
                                cache_ip[[k]] <- ip_k
                            } else {
                                cache_ip[k] <- list(NULL)
                            }
                        }
                    } else {
                        cache_str[k] <- ""
                        cache_idx[[k]] <- numeric(0)
                        cache_ip[k] <- list(NULL)
                    }
                }
                if (do_skip) {
                    has_quotes <- has_quotes[do_more]
                    do_skip <- any(has_quotes)
                }
            } else {
                n_work <- 0L
            }
        }
        idx_len <- lengths(str_idx, use.names = FALSE)
        len_m1 <- pmax(idx_len - 1L, 0L)
        nas <- list(list(rep.int(NA_real_, nchar(k_mail_sep))))
        idx_seq <-
            mapply(c, mapply(rbind, lapply(len_m1, seq_len),
                             mapply(rep.int, idx_len + 1, len_m1,
                                    SIMPLIFY = FALSE, USE.NAMES = FALSE),
                             SIMPLIFY = FALSE, USE.NAMES = FALSE),
                   ifelse(idx_len > 0L, idx_len, list(NULL)),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
        clean_strings <- vapply(str2, paste0, "",
                                collapse = k_mail_sep, USE.NAMES = FALSE)
        location_maps <-
            lapply(mapply(`[`, mapply(c, str_idx, nas, SIMPLIFY = FALSE,
                                      USE.NAMES = FALSE),
                          idx_seq, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                   unlist, recursive = FALSE, use.names = FALSE)
        list(text = clean_strings, orig.idx = location_maps)
    } # end of remove_urls
    ## comment_is_bad: Tell if a comment in an email address is in a
    ## legal position. Comments are allowed around "@" or its
    ## obfuscated version "at". Comments at the beginning and end of
    ## an address are not considered here, but they are allowed.
    if (deobfuscate) {
        comment_is_bad <- function(before, after) {
            !(grepl("@[[:space:]]*+$", before, perl = TRUE) ||
              grepl("^[[:space:]]*+@", after, perl = TRUE) ||
              grepl("[[:space:]][aA][tT][[:space:]]++$",
                    before, perl = TRUE) ||
              grepl("^[[:space:]]++[aA][tT][[:space:]]",
                    after, perl = TRUE) ||
              (grepl("[[:space:]][aA][tT]$", before, perl = TRUE) &&
               grepl("^[[:space:]]", after, perl = TRUE)) ||
              (grepl("^[aA][tT][[:space:]]", after, perl = TRUE) &&
               grepl("[[:space:]]$", before, perl = TRUE)))
        }
    } else {
        comment_is_bad <- function(before, after) {
            !(grepl("@[[:space:]]*+$", before, perl = TRUE) ||
              grepl("^[[:space:]]*+@", after, perl = TRUE))
        }
    }
    ## update_stack: Keeps track of matching brackets. Updates
    ## 'char_stack' (vector of individual characters) with 'chrs'
    ## (must only contain "(", ")", "[", or "]"). If success, returns
    ## the modified stack. If failure, returns the last point at which
    ## the brackets were balanced (or NA).
    update_stack <- function(char_stack, chrs) {
        left_chars <- c(")" = "(", "]" = "[")
        stack_len <- length(char_stack)
        stack2 <- char_stack
        zero_point <- NA_real_
        for (l in seq_along(chrs)) {
            this_char <- chrs[l]
            if (this_char %in% left_chars) {
                stack2 <- c(stack2, this_char)
                stack_len <- stack_len + 1
            } else if (stack_len > 0 &&
                       stack2[stack_len] == left_chars[[this_char]]) {
                stack2 <- stack2[-stack_len]
                stack_len <- stack_len - 1
                if (stack_len == 0) {
                    zero_point <- l
                }
            } else {
                return(zero_point)
            }
        }
        structure(stack2, zero.point = zero_point)
    }
    ## rm_brackets: Removes (some) matching initial and trailing (),
    ## []. This is done across string boundaries. Curly brackets ("{",
    ## "}") have already been removed.
    rm_brackets <- function(strings) {
        left_punct <- sub("^([[:punct:]]*).*$", "\\1", strings)
        left <- nzchar(left_punct)
        if (!any(left)) {
            return(strings)
        }
        left_brackets <-
            sub("^([]()[]*+).*$", "\\1", left_punct, perl = TRUE)
        any_left_brac <- nzchar(left_brackets)
        if (!any(any_left_brac) ||
             any(grepl("[]()[]", left_punct[!any_left_brac],
                       perl = TRUE))) {
            return(strings)
        }
        n_strings <- length(strings)
        ## 'save_brackets': number of trailing brackets not to be removed
        save_brackets <- numeric(n_strings)
        right_brackets <- sub("^.*?([]()[]*+)$", "\\1", strings, perl=TRUE)
        any_right_brac <- nzchar(right_brackets)
        only_brac <-
            which(any_right_brac)[nchar(right_brackets[any_right_brac]) ==
                                  nchar(strings[any_right_brac])]
        any_right_brac[only_brac] <- FALSE
        ## Balanced brackets within a string (excluding initial
        ## punctuation) are protected
        for (k in which(any_right_brac)) {
            if (left[k]) {
                tmp_string <- substr(strings[k], nchar(left_punct[k]) + 1,
                                     nchar(strings[k]))
            } else {
                tmp_string <- strings[k]
            }
            every_brac <-
                strsplit(gsub("[^]()[]+", "", tmp_string, perl = TRUE),
                         "")[[1L]]
            tmp_stack <- update_stack(character(0), every_brac)
            if (is.numeric(tmp_stack)) {
                zero_point <- tmp_stack
            } else {
                zero_point <- attr(tmp_stack, "zero.point")
            }
            if (!is.na(zero_point)) {
                rb <- right_brackets[k]
                n_brac <- nchar(rb)
                n_rm <- n_brac - length(every_brac) + zero_point
                if (n_rm == n_brac) {
                    right_brackets[k] <- ""
                    any_right_brac[k] <- FALSE
                } else if (n_rm > 0L) {
                    right_brackets[k] <- substr(rb, 1 + n_rm, n_brac)
                    save_brackets[k] <- n_rm
                }
            }
        }
        brac_stack <- character(0)
        work <- which(any_left_brac | any_right_brac)
        stack_lengths <- rep.int(-1L, n_strings)
        ## Find matching brackets
        for (k in work) {
            if (any_left_brac[k]) {
                brac_stack <-
                    update_stack(brac_stack,
                                 strsplit(left_brackets[k], "")[[1L]])
            }
            if (is.character(brac_stack) && any_right_brac[k]) {
                chars <- strsplit(right_brackets[k], "")[[1L]]
                n_chars <- length(chars)
                ## Brackets on the right side may belong in the URL
                for (first_char in seq_len(n_chars)) {
                    tmp_stack <-
                        update_stack(brac_stack,
                                     chars[seq.int(first_char, n_chars)])
                    if (!is.numeric(tmp_stack)) {
                        break
                    }
                }
                brac_stack <- tmp_stack
                if (is.numeric(brac_stack)) {
                    save_brackets[k] <- save_brackets[k] + n_chars
                } else {
                    save_brackets[k] <- save_brackets[k] + first_char - 1
                }
            }
            if (is.numeric(brac_stack)) {
                break
            }
            stack_lengths[k] <- length(brac_stack)
        }
        ## Select strings to edit
        if (is.character(brac_stack) && length(brac_stack) == 0L) {
            rm_brac <- work
        } else {
            length_zero <- which(stack_lengths == 0L)
            n_zeros <- length(length_zero)
            if (n_zeros > 0L) {
                rm_brac <- work[work <= length_zero[n_zeros]]
            } else {
                rm_brac <- integer(0)
            }
        }
        ## Remove selected brackets
        strings2 <- strings
        if (length(rm_brac) > 0L) {
            rm_left <- rm_brac[any_left_brac[rm_brac]]
            strings2[rm_left] <-
                sub("^[]()[]+", "", strings[rm_left], perl = TRUE)
            rm_right <- rm_brac[any_right_brac[rm_brac]]
            k_repeat <- "([]()[]{%.0f})[]()[]*+$"
            for (k in rm_right) {
                strings2[k] <- sub(sprintf(k_repeat, save_brackets[k]),
                                   "\\1", strings2[k], perl = TRUE)
            }
        }
        strings2
    } # end of rm_brackets
    ## rm_punct: Removes matching initial and trailing punctuation
    ## characters from 'strings', one string at a time. If a match is
    ## found, also other initial punctuation is removed.
    rm_punct <- function(strings) {
        left_punct <- sub("^([[:punct:]]*).*$", "\\1", strings)
        n_punct <- nchar(left_punct)
        work <- which(n_punct > 0L)
        if (length(work) == 0L) {
            return(strings)
        }
        strings2 <- strings
        expect_punct <-
            vapply(left_punct[work], rev_brackets, "", USE.NAMES = FALSE)
        ns <- nchar(strings[work])
        for (k in seq_along(work)) {
            item <- work[k]
            np_it <- n_punct[item]
            string <- strings[item]
            n <- ns[k]
            ep <- expect_punct[k]
            for (np in seq.int(from = np_it, by = -1L,
                               length.out = np_it)) {
                if (substr(string, n - np + 1L, n) ==
                    substr(ep, 1L, np)) {
                    strings2[item] <- substr(string, np_it + 1, n - np)
                    break
                }
            }
        }
        strings2
    } # end of rm_punct
    ## rm_trailing: Remove each ".", "?" and "!" that ends a sentence
    ## in string. Additional requirements for a sentence:
    ## * Starts the string or trails another sentence (separated by
    ##   space),
    ## * does not start with a lower-case letter, and
    ## * contains at least one word consisting of letters only (may
    ##   end with a punctuation character).
    ## If there are multiple candidate locations where a sentence
    ## could end, the last one prevails, unless there is a lone ".",
    ## "?", or "!", The first of which is considered to end the
    ## sentence. Arguments 'capital' and 'proper' and the 2nd and 3rd
    ## return values preserve state between consecutive strings.
    rm_trailing <- function(string, capital = FALSE, proper = FALSE) {
        reg_end <- gregexpr(k_end_sentence, string, perl = TRUE)[[1L]]
        if (reg_end[1L] == -1L) {
            new_proper <- proper
            if (capital) {
                if (!new_proper) {
                    new_proper <- grepl(k_proper, string, perl = TRUE)
                }
                return(list(string, TRUE, new_proper))
            }
            reg_end <- numeric(0)
        }
        reg_before <- gregexpr(k_before_word, string, perl = TRUE)[[1L]]
        idx_begin <- which(reg_before %in% c(1, reg_end + 1))
        loc_begin <- reg_before[idx_begin] +
            attr(reg_before, "match.length")[idx_begin]
        if (length(loc_begin) > 0L) {
            char_begin <- substring(string, loc_begin, loc_begin)
            idx_notlower <- which(toupper(char_begin) == char_begin)
            loc_begin <- loc_begin[idx_notlower]
        }
        if (capital && (length(loc_begin) == 0L || loc_begin[1L] != 1)) {
            min_begin <- 1
            loc_begin <- c(1, loc_begin)
        } else if (length(loc_begin) > 0L) {
            min_begin <- min(loc_begin)
        }
        if (length(loc_begin) > 0L) {
            loc_end <- reg_end[reg_end > min_begin]
            new_capital <- TRUE
        } else {
            loc_end <- NULL
            new_capital <- FALSE
        }
        nc <- nchar(string)
        if (length(loc_end) == 0L) {
            new_proper <- proper
            if (new_capital && !new_proper) {
                new_proper <-
                    grepl(k_proper, substr(string, min_begin, nc),
                          perl = TRUE)
            }
            return(list(string, new_capital, new_proper))
        }
        loc_begin <- c(loc_begin, Inf)
        begin_then_end <- vapply(loc_end, `>`, logical(length(loc_begin)),
                                 loc_begin, USE.NAMES = FALSE)
        map_end_to_begin <- apply(begin_then_end, 2, which.min) - 1L
        end_m1 <- loc_end - 1L
        lone_punct <- substring(string, end_m1, end_m1) == " "
        if (any(lone_punct)) {
            keep_end <- rep.int(TRUE, length(loc_end))
            for (this_begin in unique(map_end_to_begin[lone_punct])) {
                ends <- which(map_end_to_begin == this_begin)
                dont_keep <- ends[ends > min(ends[lone_punct[ends]])]
                keep_end[dont_keep] <- FALSE
            }
            loc_end <- loc_end[keep_end]
            map_end_to_begin <- map_end_to_begin[keep_end]
        }
        no_dupe <- which(rev(!duplicated(rev(map_end_to_begin))))
        loc_end <- loc_end[no_dupe]
        next_begin <- loc_begin[length(loc_end) + 1]
        new_capital <- is.finite(next_begin)
        loc_begin <- loc_begin[seq_along(loc_end)]
        has_proper_word <-
            grepl(k_proper, substring(string, loc_begin, loc_end),
                  perl = TRUE)
        if (proper) {
            has_proper_word[1L] <- TRUE
        }
        loc_end <- loc_end[has_proper_word]
        new_proper <- new_capital
        if (new_proper) {
            new_proper <- grepl(k_proper, substr(string, next_begin, nc),
                                perl = TRUE)
        }
        list(paste0(substring(string, c(1, loc_end + 1),
                              c(loc_end - 1L, nc)), collapse = ""),
             new_capital, new_proper)
    } # end of rm_trailing
    ## split_invalid: Silently flag missing strings and strings with
    ## "bytes" or invalid encoding for removal. However, salvage
    ## (presumed) ASCII substrings fulfilling certain conditions when
    ## locale is UTF-8 and encoding is unknown or declared encoding is
    ## UTF-8.
    split_invalid <- function(x) {
        enc <- Encoding(x)
        keep_flag <- !is.na(x) & enc != "bytes"
        keep_idx <- which(keep_flag)
        ve <- validEnc(x[keep_idx])
        bad_idx <- keep_idx[!ve]
        if (length(bad_idx) == 0L) {
            return(list(x, keep_flag, keep_idx))
        }
        enc_bad <- enc[bad_idx]
        if (l10n_info()[["UTF-8"]]) {
            enc_try <- enc_bad %in% c("UTF-8", "unknown")
        } else {
            enc_try <- enc_bad == "UTF-8"
        }
        split_idx <- bad_idx[enc_try]
        keep_flag[bad_idx] <- FALSE
        n_split <- length(split_idx)
        keep_split <- vector(mode = "list", length = n_split)
        parts_split <- keep_split
        for (k in seq_len(n_split)) {
            idx_k <- split_idx[k]
            x_k <- x[idx_k]
            Encoding(x_k) <- "bytes"
            raw_k <- charToRaw(x_k)
            n_bytes <- length(raw_k)
            ascii_diff <- diff(c(0L, raw_k <= 127L, 0L))
            ascii_loc <- which(ascii_diff == 1L)
            n_loc <- length(ascii_loc)
            if (n_loc == 0L) {
                parts_split[[k]] <- ""
                keep_split[[k]] <- FALSE
            } else {
                ascii_last <- which(ascii_diff == -1L) - 1L
                ascii_parts <- substring(x_k, ascii_loc, ascii_last)
                Encoding(ascii_parts) <- "unknown" # probably unnecessary
                keep_part <- grepl("[^[:space:][:cntrl:]]", ascii_parts,
                                   perl = TRUE)
                inner_flag <- rep.int(TRUE, n_loc)
                if (ascii_loc[1L] == 1L) {
                    inner_flag[1L] <- FALSE
                }
                if (ascii_last[n_loc] == n_bytes) {
                    inner_flag[n_loc] <- FALSE
                }
                keep_part[inner_flag] <- keep_part[inner_flag] &
                    grepl("[^[:space:][:cntrl:]]{2}",
                          ascii_parts[inner_flag], perl = TRUE)
                n_loc <- sum(as.numeric(keep_part))
                if (n_loc == 0) {
                    parts_split[[k]] <- ""
                    keep_split[[k]] <- FALSE
                    next
                } else if (n_loc < length(ascii_loc)) {
                    ascii_loc <- ascii_loc[keep_part]
                    ascii_last <- ascii_last[keep_part]
                    ascii_parts <- ascii_parts[keep_part]
                }
                parts_k <- as.vector(rbind(ascii_parts, ""))[-2 * n_loc]
                if (ascii_loc[1L] != 1L) {
                    parts_k <- c("", parts_k)
                }
                if (ascii_last[n_loc] != n_bytes) {
                    parts_k <- c(parts_k, "")
                }
                keep_split[[k]] <- nzchar(parts_k)
                parts_split[[k]] <- parts_k
            }
        }
        if (any(lengths(keep_split) != 1L)) {
            x2 <- as.list(x)
            x2[split_idx] <- parts_split
            x2 <- unlist(x2)
            keep_flag <- as.list(keep_flag)
            keep_flag[split_idx] <- keep_split
            keep_flag <- unlist(keep_flag)
            list(x2, keep_flag, which(keep_flag))
        } else {
            list(x, keep_flag, which(keep_flag))
        }
    } # end of split_invalid
    opens_quote_multi <- function(strings) {
        quote_match <- gregexpr(k_quote, strings, perl = TRUE)
        quote_count <- lengths(quote_match, use.names = FALSE)
        quote_count[vapply(quote_match, `[`, 0, 1,
                           USE.NAMES = FALSE) == -1] <- 0
        result <- logical(length(strings))
        idx_odd <- which(quote_count %% 2 == 1)
        if (length(idx_odd) > 0L) {
            last_quote <- mapply(`[`, quote_match[idx_odd],
                                 quote_count[idx_odd], USE.NAMES = FALSE)
            subs_odd <- strings[idx_odd]
            subs_odd <- substr(subs_odd, last_quote, nchar(subs_odd))
            result[idx_odd] <- grepl(k_qs0, subs_odd, perl=TRUE)
        }
        result
    }
    opens_quote_single <- function(string) {
        quote_match <- gregexpr(k_quote, string, perl = TRUE)[[1L]]
        if (quote_match[1L] == -1L) {
            return(FALSE)
        }
        quote_count <- length(quote_match)
        if (quote_count %% 2L == 1L) {
            grepl(k_qs0, substr(string, quote_match[quote_count],
                                nchar(string)), perl = TRUE)
        } else {
            FALSE
        }
    }
    ## end of helper functions

    ## Preprocess input strings ------------------------------------------
    x2 <- as.character(x)
    tmp <- split_invalid(x2)
    ## Convert to UTF-8 for more predictable string ops, e.g. in C locale
    x2 <- enc2utf8(tmp[[1L]])
    keep_flag <- tmp[[2L]]
    keep_idx <- tmp[[3L]]
    n_x2 <- length(keep_idx)
    if (n_x2 == 0L) {
        if (plain_email2) {
            return(list(url = character(0), email = character(0)))
        } else {
            return(character(0))
        }
    }
    break_flag <- logical(length(x2))
    break_flag[keep_idx[c(diff(keep_idx) != 1L, TRUE)]] <- TRUE
    ## Collapse blocks of lines (strings) so that each potential
    ## URL between angle brackets "<", ">" occupies a single
    ## (collapsed) string. Empty lines not allowed.
    collapse2 <- collapse_x && n_x2 > 1L
    if (collapse2) {
        x_keep <- x2[keep_idx]
        angle_tmp <- grep("<", x_keep, fixed = TRUE)
        which_left <- keep_idx[angle_tmp]
        which_left <-
            which_left[grep(paste0("<[[:space:]]*+", k_sch_colon,
                                   "(?:[^\"<>[:cntrl:]]|[[:space:]])*+$"),
                            x_keep[angle_tmp], perl = TRUE)]
        if (length(which_left) == 0L) {
            n_right <- 0L
        } else {
            angle_tmp <- grep(">", x_keep, fixed = TRUE)
            which_right <- keep_idx[angle_tmp]
            which_right <-
                which_right[grep("^(?:[^\"<>[:cntrl:]]|[[:space:]])*+>",
                                 x_keep[angle_tmp], perl = TRUE)]
            which_right <- which_right[which_right > min(which_left)]
            n_right <- length(which_right)
            if (n_right > 0L) {
                which_left <- which_left[which_left < max(which_right)]
            }
        }
        angle_tmp <- NULL
        angle_loc <- numeric(n_right)
        angle_last <- numeric(n_right)
        last_end <- 0
        last_k <- 0
        for (k in seq_len(n_right)) {
            this_end <- which_right[k]
            this_begin <-
                which_left[which_left >= last_end & which_left < this_end]
            n_begin <- length(this_begin)
            if (n_begin == 0) {
                last_end <- this_end
                next
            }
            this_begin <- this_begin[n_begin]
            if (any(!keep_flag[this_begin:this_end])) {
                last_end <- this_end
                next
            }
            if (this_begin != last_end) {
                last_k <- last_k + 1
                angle_loc[last_k] <- this_begin
            }
            angle_last[last_k] <- this_end
            last_end <- this_end
        }
        which_left <- NULL
        which_right <- NULL
        if (last_k == 0) {
            x2 <- x_keep
            x_keep <- NULL
            break_flag <- break_flag[keep_idx]
        } else {
            x_keep <- NULL
            tmp_seq <- seq_len(last_k)
            angle_loc <- angle_loc[tmp_seq]
            angle_last <- angle_last[tmp_seq]
            for (k in tmp_seq) {
                keep_flag[angle_loc[k]:angle_last[k]] <- FALSE
            }
            tmp_seq <- NULL
            keep_idx <- which(keep_flag)
            x2_1 <- vapply(mapply(`[`, list(x2),
                                  mapply(`:`, angle_loc, angle_last,
                                         SIMPLIFY=FALSE, USE.NAMES=FALSE),
                                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           paste0, "", collapse = "\n", USE.NAMES = FALSE)
            x_order <- order(c(angle_loc, keep_idx))
            x2 <- c(x2_1, x2[keep_idx])[x_order]
            x2_1 <- NULL
            break_flag <- break_flag[c(angle_last, keep_idx)[x_order]]
            x_order <- NULL
        }
        angle_loc <- NULL
        angle_last <- NULL
    } else {
        x2 <- x2[keep_idx]
        break_flag <- break_flag[keep_idx]
    }
    keep_flag <- NULL
    keep_idx <- NULL
    n_x2 <- length(x2)
    ## Collapse blocks of lines (strings) so that each potential plain
    ## email address occupies a single (collapsed) string: qouted
    ## strings.
    collapse2 <- collapse2 && plain_email2 && n_x2 > 1L
    if (collapse2) {
        at_flag <- grepl(k_rough_email1, x2, perl = TRUE)
        fold_flag <- grepl(k_fws1s, x2, perl = TRUE)
        at_idx <- which(at_flag)
        n_at <- length(at_idx)
        k <- 1
        if (n_at > 0L) {
            qs_flag <- logical(n_x2)
            test_idx <- which(fold_flag[-1L] & !break_flag[-n_x2])
            qs_flag[test_idx] <- opens_quote_multi(x2[test_idx])
            test_idx <- NULL
            last_at <- at_idx[n_at]
            quote_loc <- numeric(0)
            quote_last <- numeric(0)
            xk <- x2[k]
            next_break <- 0
        } else {
            last_at <- 0
            quote_loc <- NULL
        }
        at_idx <- NULL
        while (k <= last_at - 1) {
            if (k > next_break) {
                breaks <- which(break_flag[k:(last_at - 1)])
                n_breaks <- length(breaks)
                if (n_breaks == 0L) {
                    next_break <- last_at
                } else {
                    diff_2 <- which(diff(c(0, breaks)) >= 2L)[1L]
                    if (is.na(diff_2)) {
                        next_break <- last_at
                        k <- k + breaks[n_breaks]
                        if (k >= last_at) {
                            break
                        }
                        xk <- x2[k]
                    } else if (diff_2 == 1L) {
                        next_break <- k - 1 + breaks[1L]
                    } else {
                        next_break <- k - 1 + breaks[diff_2]
                        k <- k + breaks[diff_2 - 1L]
                        xk <- x2[k]
                    }
                }
                breaks <- NULL
                at_int <- which(at_flag[k:next_break])
                n_at_int <- length(at_int)
                if (n_at_int == 0L) {
                    last_at_int <- k
                } else {
                    last_at_int <- k - 1 + at_int[n_at_int]
                }
                at_int <- NULL
            }
            if (k == last_at_int) {
                k <- next_break + 1
                xk <- x2[k]
                next
            }
            next_qs <- which(qs_flag[k:(next_break - 1)])[1L]
            if (is.na(next_qs)) {
                k <- next_break + 1
                xk <- x2[k]
                next
            }
            if (next_qs != 1L) {
                k <- k - 1 + next_qs
                xk <- x2[k]
            }
            quote_loc[length(quote_loc) + 1] <- k
            l <- k + 1
            inc_k <- TRUE
            while (l <= last_at && !break_flag[l - 1]) {
                xl <- x2[l]
                reg_qs1 <- regexpr(k_qs1, xl, perl = TRUE)
                if (reg_qs1 != -1L) {
                    quote_last[length(quote_last) + 1] <- l
                    k <- l
                    xk <- x2[k]
                    xk <- substr(xk,
                                 reg_qs1 + attr(reg_qs1, "match.length"),
                                 nchar(xk))
                    qs_flag[k] <- k < last_at && !break_flag[k] &&
                        fold_flag[k + 1] && opens_quote_single(xk)
                    inc_k <- FALSE
                    break
                } else if (!grepl(k_qs2, xl, perl = TRUE)) {
                    k <- l
                    xk <- x2[k]
                    inc_k <- FALSE
                    break
                } else {
                    l <- l + 1
                }
            }
            n_loc <- length(quote_loc)
            if (n_loc > length(quote_last)) {
                quote_loc <- quote_loc[-n_loc]
            } else if (n_loc > 1L &&
                       quote_loc[n_loc] == quote_last[n_loc - 1L]) {
                quote_last[n_loc - 1L] <- quote_last[n_loc]
                quote_loc <- quote_loc[-n_loc]
                quote_last <- quote_last[-n_loc]
            }
            if (inc_k) {
                k <- k + 1
                xk <- x2[k]
            }
        }
        xk <- NULL
        n_loc <- length(quote_loc)
        if (n_loc > 0L) {
            noquote <- rep.int(TRUE, n_x2)
            at_quote <- logical(n_loc)
            for (k in seq_len(n_loc)) {
                quote_seq <- quote_loc[k]:quote_last[k]
                noquote[quote_seq] <- FALSE
                at_quote[k] <- any(at_flag[quote_seq])
            }
            noquote_idx <- which(noquote)
            noquote <- NULL
            x2_2 <- vapply(mapply(`[`, list(x2),
                                  mapply(`:`, quote_loc, quote_last,
                                         SIMPLIFY=FALSE, USE.NAMES=FALSE),
                                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           paste0, "", collapse = "\n", USE.NAMES = FALSE)
            blocks2 <- c(quote_loc, noquote_idx)
            quote_loc <- NULL
            x_order2 <- order(blocks2)
            x2 <- c(x2_2, x2[noquote_idx])[x_order2]
            x2_2 <- NULL
            break_flag <- break_flag[c(quote_last, noquote_idx)[x_order2]]
            quote_last <- NULL
            at_flag <- c(at_quote, at_flag[noquote_idx])[x_order2]
            noquote_idx <- NULL
            fold_flag <- fold_flag[blocks2[x_order2]]
            x_order2 <- NULL
            blocks2 <- NULL
        }
    }
    qs_flag <- NULL
    n_x2 <- length(x2)
    ## Collapse lines for email detection: comments in parentheses.
    collapse2 <- collapse2 && n_x2 > 1L
    if (collapse2) {
        comm_loc <- numeric(0)
        k <- 1
        next_break <- 0
        lpar_idx <- grep("(", x2, fixed = TRUE)
        n_lpar <- length(lpar_idx)
        if (n_lpar > 0L) {
            at_idx <- which(at_flag)
            n_at <- length(at_idx)
            if (n_at == 0L) {
                max_lpar <- 0
            } else {
                at_flag2 <- logical(n_x2)
                for (k in n_at:1) {
                    last_idx <- at_idx[k]
                    if (last_idx < n_x2 && !break_flag[last_idx] &&
                        fold_flag[last_idx + 1]) {
                        last_idx <- last_idx + 1
                    }
                    if (k > 1) {
                        first_idx <- at_idx[k - 1L] + 1
                    } else {
                        first_idx <- 1
                    }
                    if (last_idx > first_idx) {
                        breaks <-
                            which(break_flag[first_idx:(last_idx - 1)] |
                                  !fold_flag[(first_idx + 1):last_idx])
                        n_breaks <- length(breaks)
                        if (n_breaks > 0L) {
                            first_idx <- breaks[n_breaks] + first_idx
                        }
                        breaks <- NULL
                        at_flag2[first_idx:last_idx] <- TRUE
                    } else {
                        at_flag2[first_idx] <- TRUE
                    }
                }
                at_idx2 <- which(at_flag2)
                at_ival <- findInterval(0:(n_x2 - 1), at_idx2) + 1
                comm_last <- numeric(0)
                lpar_flag <- logical(n_x2)
                lpar_flag[lpar_idx] <- TRUE
                rpar_flag <- grepl(")", x2, fixed = TRUE)
                max_lpar <- lpar_idx[n_lpar]
                lpar_ival <- findInterval(0:(max_lpar - 1), lpar_idx) + 1
            }
            at_idx <- NULL
        } else {
            max_lpar <- 0
        }
        find_loc <- TRUE
        while (k <= max_lpar || (!find_loc && k <= n_x2)) {
            if (k > next_break) {
                breaks <- which(break_flag[k:n_x2])
                diff_2 <- which(diff(c(0, breaks)) >= 2L)[1L]
                if (is.na(diff_2)) {
                    next_break <- n_x2
                    k <- k + breaks[length(breaks)]
                    if (k > n_x2) {
                        break
                    }
                } else if (diff_2 == 1L) {
                    next_break <- k - 1 + breaks[1L]
                } else {
                    next_break <- k - 1 + breaks[diff_2]
                    k <- k + breaks[diff_2 - 1L]
                }
                breaks <- NULL
                next_fold <-
                    which(fold_flag[seq.int(from = k + 1, by = 1,
                                            length.out = next_break - k)])
                if (length(next_fold) == 0L) {
                    k <- next_break + 1
                } else {
                    next_fold <- next_fold[1L]
                    k <- k - 1 + next_fold
                    level <- 0
                    after_break <- next_fold == 1L
                }
                next
            }
            doit <- TRUE
            if (!find_loc && !lpar_flag[k]) {
                if (!fold_flag[k]) {
                    new_level <- 0
                    all_comm <- FALSE
                    end_comm <- FALSE
                    begin_comm <- FALSE
                    doit <- FALSE
                } else if (!rpar_flag[k]) {
                    new_level <- level
                    all_comm <- FALSE
                    end_comm <- new_level > 0
                    begin_comm <- TRUE
                    doit <- FALSE
                }
            }
            if (doit) {
                if (find_loc && !lpar_flag[k]) {
                    nb_p1A <- next_break + 1
                    k <- lpar_idx[lpar_ival[k]]
                    if (k >= nb_p1A) {
                        k <- nb_p1A
                        next
                    }
                    after_break <- FALSE
                }
                if (find_loc && !at_flag2[k]) {
                    nb_p1 <- next_break + 1
                    k <- at_idx2[at_ival[k]]
                    if (is.na(k) || k >= nb_p1) {
                        k <- nb_p1
                        next
                    }
                    after_break <- FALSE
                }
                tmp <- count_comments(x2[k], level)
                new_level <- tmp[[1L]]
                all_comm <- isTRUE(tmp[[2L]])
                end_comm <- isTRUE(tmp[[3L]])
                begin_comm <- isTRUE(tmp[[4L]])
            }
            this_last <- NULL
            if (find_loc) {
                if (begin_comm) {
                    if (!after_break) {
                        comm_loc[length(comm_loc) + 1] <- k - 1
                        find_loc <- FALSE
                    }
                } else if (break_flag[k]) {
                    k <- k + 1
                    next
                } else if (end_comm && !all_comm) {
                    comm_loc[length(comm_loc) + 1] <- k
                    find_loc <- FALSE
                }
            }
            if (!find_loc) {
                n_loc <- length(comm_loc)
                if (!begin_comm && !end_comm) {
                    this_last <- k - !fold_flag[k]
                    if (this_last == comm_loc[n_loc]) {
                        comm_loc <- comm_loc[-n_loc]
                        this_last <- NULL
                    }
                    find_loc <- TRUE
                } else if (break_flag[k] || !end_comm) {
                    this_last <- k
                    find_loc <- TRUE
                }
            }
            level <- new_level
            k <- k + 1
            after_break <- FALSE
            if (!is.null(this_last)) {
                n_loc <- length(comm_loc)
                n_last <- n_loc - 1
                if (n_last > 0L &&
                    comm_last[n_last] == comm_loc[n_loc]) {
                    comm_loc <- comm_loc[-n_loc]
                    comm_last[n_last] <- this_last
                } else {
                    comm_last[n_loc] <- this_last
                }
            }
        }
        at_ival <- NULL
        lpar_ival <- NULL
        at_flag2 <- NULL
        lpar_idx <- NULL
        at_idx2 <- NULL
        n_loc <- length(comm_loc)
        if (n_loc > 0L) {
            nocomm <- rep.int(TRUE, n_x2)
            at_comm <- logical(n_loc)
            for (k in seq_along(comm_loc)) {
                comm_seq <- comm_loc[k]:comm_last[k]
                nocomm[comm_seq] <- FALSE
                at_comm[k] <- any(at_flag[comm_seq])
            }
            comm_seq <- NULL
            nocomm_idx <- which(nocomm)
            nocomm <- NULL
            x2_3 <- vapply(mapply(`[`, list(x2),
                                  mapply(`:`, comm_loc, comm_last,
                                         SIMPLIFY=FALSE, USE.NAMES=FALSE),
                                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           paste0, "", collapse = "\n", USE.NAMES = FALSE)
            blocks3 <- c(comm_loc, nocomm_idx)
            comm_loc <- NULL
            x_order3 <- order(blocks3)
            x2 <- c(x2_3, x2[nocomm_idx])[x_order3]
            x2_3 <- NULL
            break_flag <- break_flag[c(comm_last, nocomm_idx)[x_order3]]
            comm_last <- NULL
            at_flag <- c(at_comm, at_flag[nocomm_idx])[x_order3]
            at_comm <- NULL
            nocomm_idx <- NULL
            fold_flag <- fold_flag[blocks3[x_order3]]
            x_order3 <- NULL
            blocks3 <- NULL
        }
    }
    lpar_flag <- NULL
    rpar_flag <- NULL
    n_x2 <- length(x2)
    ## Collapse lines for email detection: space around "@" / "[aA][tT]".
    collapse2 <- collapse2 && n_x2 > 1L
    if (collapse2) {
        start_space <- fold_flag[-1L]
        ats <- grepl(paste0("@", k_fws0, "$"), x2[-n_x2],
                     perl=TRUE) & start_space
        ats <- ats | grepl(paste0(k_fws1s, "@"), x2[-1L], perl = TRUE)
        if (deobfuscate) {
            ats <- ats | (grepl(paste0(k_fws1, "[aA][tT]", k_fws0, "$"),
                                x2[-n_x2], perl=TRUE) & start_space)
            ats <- ats | grepl(paste0(k_fws1s, "[aA][tT](?:", k_fws1,
                                      "|$)"), x2[-1L], perl = TRUE)
        }
        start_space <- NULL
        ats <- c(ats, FALSE) & !break_flag
        if (any(ats)) {
            diff_ats <- diff(c(FALSE, ats))
            ats <- NULL
            at_loc <- which(diff_ats == 1L)
            n_loc <- length(at_loc)
            at_last <- which(diff_ats == -1L)
            diff_ats <- NULL
            noat <- rep.int(TRUE, n_x2)
            for (k in seq_len(n_loc)) {
                noat[at_loc[k]:at_last[k]] <- FALSE
            }
            noat_idx <- which(noat)
            noat <- NULL
            x2_4 <- vapply(mapply(`[`, list(x2),
                                  mapply(`:`, at_loc, at_last,
                                         SIMPLIFY=FALSE, USE.NAMES=FALSE),
                                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           paste0, "", collapse = "\n", USE.NAMES = FALSE)
            blocks4 <- c(at_loc, noat_idx)
            at_loc <- NULL
            x_order4 <- order(blocks4)
            x2 <- c(x2_4, x2[noat_idx])[x_order4]
            x2_4 <- NULL
            break_flag <- break_flag[c(at_last, noat_idx)[x_order4]]
            at_last <- NULL
            at_flag <- c(rep.int(TRUE, n_loc),
                         at_flag[noat_idx])[x_order4]
            noat_idx <- NULL
            fold_flag <- fold_flag[blocks4[x_order4]]
            x_order4 <- NULL
            blocks4 <- NULL
        } else {
            ats <- NULL
        }
    }
    n_x2 <- length(x2)
    ## Collapse lines for email detection: space inside domain-literal
    collapse2 <- collapse2 && n_x2 > 1L
    if (collapse2) {
        lit_loc <- numeric(0)
        k <- 1
        next_break <- 0
        lbrac_idx <- grep("[", x2, fixed = TRUE)
        n_lbrac <- length(lbrac_idx)
        if (n_lbrac > 0L) {
            lit_last <- numeric(0)
            lbrac_flag <- logical(n_x2)
            lbrac_flag[lbrac_idx] <- TRUE
            max_lbrac <- lbrac_idx[n_lbrac]
            lbrac_ival <- findInterval(0:(max_lbrac - 1), lbrac_idx) + 1
        } else {
            max_lbrac <- 0
        }
        find_loc <- TRUE
        while (k <= max_lbrac || (!find_loc && k <= n_x2)) {
            if (k > next_break) {
                breaks <- which(break_flag[k:n_x2])
                diff_2 <- which(diff(c(0, breaks)) >= 2L)[1L]
                if (is.na(diff_2)) {
                    next_break <- n_x2
                    k <- k + breaks[length(breaks)]
                    if (k > n_x2) {
                        break
                    }
                } else if (diff_2 == 1L) {
                    next_break <- k - 1 + breaks[1L]
                } else {
                    next_break <- k - 1 + breaks[diff_2]
                    k <- k + breaks[diff_2 - 1L]
                }
                breaks <- NULL
                if (!any(at_flag[k:next_break])) {
                    k <- next_break + 1
                    next
                }
                next_fold <-
                    which(fold_flag[seq.int(from = k + 1, by = 1,
                                            length.out = next_break - k)])
                if (length(next_fold) == 0L) {
                    k <- next_break + 1
                } else {
                    next_fold <- next_fold[1L]
                    k <- k - 1 + next_fold
                    after_break <- next_fold == 1L
                }
                next
            }
            if (find_loc && !lbrac_flag[k]) {
                nb_p1B <- next_break + 1
                k <- lbrac_idx[lbrac_ival[k]]
                if (k >= nb_p1B) {
                    k <- nb_p1B
                    next
                }
                after_break <- FALSE
            }
            xk <- x2[k]
            if (find_loc) {
                if (!break_flag[k] &&
                     grepl(k_lit0, xk, perl = TRUE)) {
                    lit_loc[length(lit_loc) + 1] <- k
                    find_loc <- FALSE
                }
            } else if (grepl(k_lit1, xk, perl = TRUE)) {
                n_loc <- length(lit_loc)
                n_last <- n_loc - 1
                if (n_last > 0L &&
                    lit_last[n_last] == lit_loc[n_loc]) {
                    lit_loc <- lit_loc[-n_loc]
                    lit_last[n_last] <- k
                } else {
                    lit_last[n_loc] <- k
                }
                if (!break_flag[k] && grepl(k_lit0, xk, perl = TRUE)) {
                    lit_loc[length(lit_loc) + 1] <- k
                } else {
                    find_loc <- TRUE
                }
            } else if (break_flag[k]) {
                lit_loc <- lit_loc[-length(lit_loc)]
                find_loc <- TRUE
            } else if (!grepl(k_lit2, xk, perl = TRUE)) {
                if (grepl(k_lit0, xk, perl = TRUE)) {
                    lit_loc[length(lit_loc)] <- k
                } else {
                    lit_loc <- lit_loc[-length(lit_loc)]
                    find_loc <- TRUE
                }
            }
            k <- k + 1
            after_break <- FALSE
        }
        xk <- NULL
        lbrac_ival <- NULL
        lbrac_idx <- NULL
        n_loc <- length(lit_loc)
        if (n_loc > 0L) {
            nolit <- rep.int(TRUE, n_x2)
            for (k in seq_along(lit_loc)) {
                nolit[lit_loc[k]:lit_last[k]] <- FALSE
            }
            nolit_idx <- which(nolit)
            nolit <- NULL
            x2_5 <- vapply(mapply(`[`, list(x2),
                                  mapply(`:`, lit_loc, lit_last,
                                         SIMPLIFY=FALSE, USE.NAMES=FALSE),
                                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           paste0, "", collapse = "\n", USE.NAMES = FALSE)
            x_order5 <- order(c(lit_loc, nolit_idx))
            lit_loc <- NULL
            x2 <- c(x2_5, x2[nolit_idx])[x_order5]
            x2_5 <- NULL
            break_flag <- break_flag[c(lit_last, nolit_idx)[x_order5]]
            nolit_idx <- NULL
            lit_last <- NULL
            x_order5 <- NULL
        }
    }
    at_flag <- NULL
    fold_flag <- NULL
    lbrac_flag <- NULL
    n_x2 <- length(x2)
    ## Find delimited URLs -----------------------------------------------
    ## 1. angle brackets "<" and ">"
    angle_loc2 <- gregexpr(paste0("<[[:space:]]*+", k_sch_colon,
                                  "(?:[^\"<>[:cntrl:]]|[[:space:]])*+>"),
                           x2, perl = TRUE)
    ## 2. double quotes
    quote_loc2 <- gregexpr(paste0("\"", k_sch_colon,
                                  "[^\"<>[:cntrl:][:space:]]*+\""),
                           x2, perl = TRUE)
    other_part <- x2
    if (single_item) {
        delim_part <- character(n_x2)
        url_delim <- logical(n_x2)
    } else {
        delim_part <- vector(mode = "list", length = n_x2)
    }
    for (k in seq_len(n_x2)) {
        item1 <- angle_loc2[[k]]
        item2 <- quote_loc2[[k]]
        has_angle <- item1[1L] != -1L
        has_quote <- item2[1L] != -1L
        if (!has_angle && !has_quote) {
            next
        }
        xk <- x2[k]
        nc_k <- nchar(xk)
        item <- numeric(0)
        after_match <- numeric(0)
        delp <- character(0)
        if (has_angle) {
            item <- item1
            after_match <- item1 + attr(item1, "match.length")
            delp <- gsub("[[:space:]]+", "",
                         substring(xk, item + 1, after_match - 2),
                         perl = TRUE)
        }
        if (has_quote) {
            am_this <- item2 + attr(item2, "match.length")
            delp_this <- substring(xk, item2 + 1, am_this - 2)
            ## A quoted URL may be the local part of an email address
            if (plain_email2) {
                test_end <- c(item2[-1L] - 1, nc_k)
                work <- grep(k_rough_email0,
                             substring(xk, am_this - 1, test_end),
                             perl = TRUE)
                if (length(work) > 0L) {
                    pe1 <- pick_email(substring(xk, item2[work],
                                                test_end[work]),
                                      single_email = FALSE,
                                      url_check = FALSE,
                                      deobfuscate = FALSE)[[1L]]
                    nz <- lengths(pe1, use.names = FALSE) > 0L
                    work <- work[nz]
                    if (length(work) > 0L) {
                        pe2 <- pick_email(substring(xk, item2[work] + 1,
                                                    test_end[work]),
                                          single_email = FALSE,
                                          url_check = FALSE,
                                          deobfuscate = FALSE)[[1L]]
                        drop <- mapply(identical, pe1[nz], pe2,
                                       SIMPLIFY = TRUE, USE.NAMES = FALSE)
                        idx_email <- work[!drop]
                        if (length(idx_email) > 0L) {
                            item2 <- item2[-idx_email]
                            has_quote <- length(item2) > 0L
                            if (!has_angle && !has_quote) {
                                next
                            }
                            am_this <- am_this[-idx_email]
                            delp_this <- delp_this[-idx_email]
                        }
                    }
                }
            }
            item <- c(item, item2)
            after_match <- c(after_match, am_this)
            delp <- c(delp, delp_this)
        }
        if (has_angle && has_quote) {
            item_order <- order(item)
            item <- item[item_order]
            after_match <- after_match[item_order]
            delp <- delp[item_order]
        }
        ## 'collapse' should not interfere with email address matching
        other_part[k] <- paste0(substring(xk, c(1L, after_match),
                                          c(item - 1L, nc_k)),
                                collapse = k_mail_sep)
        if (single_item) {
            if (length(delp) == 1L) {
                pou <- pick_one_url(delp)
                selected_string <- pou[[1L]]
                this_has_url <- pou[[2L]]
                pou <- NULL
            } else {
                selected_string <- ""
                this_has_url <- FALSE
                for (l in seq_along(delp)) {
                    pou <- pick_one_url(delp[l])
                    candidate <- pou[[1L]]
                    this_has_url <- pou[[2L]]
                    pou <- NULL
                    if (this_has_url) {
                        selected_string <- candidate
                        break
                    } else if (nzchar(candidate)) {
                        selected_string <- candidate
                    }
                }
            }
            delim_part[k] <- selected_string
            url_delim[k] <- this_has_url
        } else {
            delim_part[[k]] <- pick_one_plus_url(delp)
        }
    }
    item1 <- NULL
    item2 <- NULL
    item <- NULL
    am_this <- NULL
    test_end <- NULL
    angle_loc2 <- NULL
    quote_loc2 <- NULL
    delp <- NULL
    delp_this <- NULL
    xk <- NULL
    ## Pick up plain email addresses -------------------------------------
    if (single_item) {
        emails <- character(n_x2)
    } else {
        emails <- rep.int(list(character(0)), n_x2)
    }
    if (plain_email2) {
        if (single_item) {
            work <- which(!url_delim)
            work <- work[grepl(k_rough_email, other_part[work],
                               perl = TRUE)]
        } else {
            work <- grep(k_rough_email, other_part, perl = TRUE)
        }
        if (length(work) > 0L) {
            if (all_email) {
                u_check <- mail_alias
            } else {
                u_check <- TRUE
            }
            pe <- pick_email(other_part[work], single_email = single_item,
                             url_check = u_check, deobfuscate=deobfuscate)
            emails[work] <- pe[[1L]]
            other_part[work] <- pe[[2L]]
            pe <- NULL
        }
        other_part[!grepl("\\w", other_part, perl = TRUE)] <- ""
    }
    ## Process remaining input -------------------------------------------
    other_part <-
        gsub("[[:cntrl:][:space:]]+", " ", other_part, perl = TRUE)
    if (single_item) {
        all_url <- all(url_delim)
    }
    if ((!single_item || !all_url) &&
        ((is.logical(rm_endpunct) && rm_endpunct) ||
         (is.numeric(rm_endpunct) && rm_endpunct >= 1))) {
        if (is.logical(rm_endpunct) || rm_endpunct >= n_x2) {
            work <- seq_len(n_x2)
        } else {
            k_rough_endpunct <- paste0(k_sch_colon, "[^[:space:]]*",
                                       k_punct, "(?:[[:space:]]|$)")
            work <- grep(k_rough_endpunct, other_part, perl = TRUE)
            n_work <- length(work)
            if (n_work > 0L) {
                work_tmp <- vector(mode = "list", length = 2 * n_work)
                last_idx <- 0
                tmp_count <- 0
                for (k in seq_len(n_work)) {
                    this_last <- work[k]
                    this_first <- this_last - rm_endpunct + 1
                    last_p1 <- last_idx + 1
                    if (this_first <= last_p1) {
                        tmp_count <- tmp_count + 1
                        work_tmp[[tmp_count]] <- last_p1:this_last
                    } else {
                        work_tmp[[tmp_count + 1]] <- -1
                        tmp_count <- tmp_count + 2
                        work_tmp[[tmp_count]] <- this_first:this_last
                    }
                    last_idx <- this_last
                }
                work <- unlist(work_tmp[seq_len(tmp_count)])
                work_tmp <- NULL
            }
        }
        capital <- FALSE
        proper <- FALSE
        for (k in work) {
            if (k == -1) {
                capital <- FALSE
                proper <- FALSE
                next
            }
            tmp <- rm_trailing(other_part[k], capital, proper)
            other_part[k] <- tmp[[1L]]
            if (break_flag[k]) {
                capital <- FALSE
                proper <- FALSE
            } else {
                capital <- tmp[[2L]]
                proper <- tmp[[3L]]
            }
        }
        tmp <- NULL
    }
    break_flag <- NULL
    if (single_item && !all_url) {
        ## If no URL was found inside delimiters, prefer other URL. If
        ## no URL but an email address was found ('plain_email2'),
        ## take the email. If nothing was found inside delimiters and
        ## no email address was found, take anything else (see
        ## 'need_scheme' and 'url_pattern'). Otherwise, prefer the
        ## delimited contents.
        work <- which(!url_delim)
        nzop <- nzchar(other_part[work])
        ## Find one URL, throw away the rest
        pou <- pick_one_url(other_part[work[nzop]],
                            ignore_commas = FALSE, keep_punct = TRUE)
        keep_other <- logical(length(work))
        keep_other[nzop] <- pou[[2L]]
        email_found <- nzchar(emails)
        work_no_keep <- work[!keep_other]
        keep_other[!keep_other] <- !nzchar(delim_part[work_no_keep]) &
            !email_found[work_no_keep]
        other_part <- pou[[1L]][keep_other[nzop]]
        pou <- NULL
        work_remain <- work[!keep_other]
        work_email <- work_remain[email_found[work_remain]]
        emails <- emails[work_email]
        keep_delim <- rep.int(TRUE, length(url_delim))
        keep_delim[work_email] <- FALSE
        keep_delim[work[keep_other]] <- FALSE
        delim_part <- delim_part[keep_delim]
    } else if (single_item) {
        other_part <- character(0)
        emails <- character(0)
    } else {
        ## For parts not between delimiters: Always split at
        ## whitespace (now includes control characters). See
        ## definition of 'k_replace' for other split patterns.
        other_part <- unlist(strsplit(other_part, k_replace, perl = TRUE),
                             recursive = FALSE, use.names = FALSE)
        other_part <- other_part[nzchar(other_part)]
        emails <- unlist(emails, recursive = FALSE, use.names = FALSE)
        delim_part <- unlist(delim_part, recursive=FALSE, use.names=FALSE)
        delim_part <- delim_part[nzchar(delim_part)]
        if (need_scheme) {
            work <- grep(k_sch_colon, other_part, perl = TRUE)
        } else {
            comma_or_url <- grepl(",", other_part, fixed = TRUE)
            find_url <- which(!comma_or_url)
            if (length(find_url) > 0L) {
                comma_or_url[find_url] <-
                    grepl(k_sch_colon, other_part[find_url], perl = TRUE)
                work <- which(comma_or_url)
            } else {
                work <- seq_along(other_part)
            }
        }
        if (length(work) > 0L) {
            other_part <- as.list(other_part)
            ## split_and_check may return URLs or other strings which
            ## need filtering
            other_part[work] <- lapply(other_part[work], split_and_check,
                                       split_userinfo = FALSE,
                                       cut_comma_email = FALSE)
            other_part <-
                unlist(other_part, recursive = FALSE, use.names = FALSE)
        }
    }
    work <- NULL
    ## Post-process ------------------------------------------------------
    ## Delimited URL or not, equal treatment after this if-else.
    if (single_item) {
        x2 <- c(delim_part, other_part)
    } else {
        x2 <- c(delim_part, rm_brackets(other_part))
    }
    delim_part <- NULL
    other_part <- NULL
    ## Find URI scheme and following ":". Remove anything before the
    ## scheme and normalize scheme to lowercase. Also replace
    ## unofficial scheme-like strings with the official scheme,
    ## e.g. "email:" -> "mailto:". No changes if scheme not found.
    scheme_idx <- grep(k_sch_colon, x2, perl = TRUE)
    any_schemes <- length(scheme_idx) > 0L
    if (any_schemes) {
        if (need_scheme) {
            x2 <- rm_punct(x2[scheme_idx])
            x_tmp <- x2
        } else {
            x2 <- rm_punct(x2)
            x_tmp <- x2[scheme_idx]
        }
        scheme_loc <-
            regexpr(paste0(k_scheme, "+(?=:)"), x_tmp, perl = TRUE)
        colon_loc <- scheme_loc + attr(scheme_loc, "match.length")
        after_scheme <- substr(x_tmp, colon_loc, nchar(x_tmp))
        old_sch <- tolower(substr(x_tmp, scheme_loc, colon_loc - 1L))
        scheme_loc <- NULL
        colon_loc <- NULL
        x_tmp <- NULL
        new_sch <- old_sch
        for (k in nonzero_sub) {
            other_names <- scheme_sub[[k]]
            other_names <- other_names[!is.na(other_names)]
            other_names <- other_names[Encoding(other_names) != "bytes"]
            new_sch[old_sch %in% tolower(other_names)] <- official_sch[k]
        }
        if (need_scheme) {
            x2 <- paste0(new_sch, after_scheme)
        } else {
            x2[scheme_idx] <- paste0(new_sch, after_scheme)
            x2 <- x2[nzchar(x2)]
        }
        scheme_idx <- NULL
        new_sch <- NULL
        after_scheme <- NULL
        old_sch <- NULL
    } else if (need_scheme) {
        x2 <- character(0)
    } else {
        x2 <- rm_punct(x2)
        x2 <- x2[nzchar(x2)]
    }
    if (all_email) {
        x2 <- x2[is.na(match(x2, paste0(mail_alias, ":")))]
    }
    if (url_pattern_exists) {
        x2 <- grep(url_pattern, x2, value = TRUE, perl = TRUE)
    }
    if (any_schemes && !single_item) {
        idx_mailto <- grep("^mailto:", x2, perl = TRUE)
        if (length(idx_mailto) > 0L) {
            x2[idx_mailto] <-
                pick_one_url(x2[idx_mailto], only_url = TRUE)[[1L]]
            rm_idx <- idx_mailto[!nzchar(x2[idx_mailto])]
            if (length(rm_idx) > 0L) {
                x2 <- x2[-rm_idx]
            }
        }
    }
    if (plain_email2) {
        if (email_pattern_exists) {
            emails <-
                grep(email_pattern, emails, value = TRUE, perl = TRUE)
        }
        list(url = x2, email = emails)
    } else {
        x2
    }
}
