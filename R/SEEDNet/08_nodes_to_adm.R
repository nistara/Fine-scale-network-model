#' Join simulation nodes with WHO adm regions
#'
#' @param outbrk_TS Time-step dataframes for each simulation
#' @param net_pts_join Network point dataframe containing country and WHO information
#' mapped to each node name.
#' 
#' @export
get_outbrks_adm = function(outbrk_TS, net_pts_join) {
    
    TS_join = dplyr::left_join(outbrk_TS, net_pts_join, by = c("name" = "cat"))

    as.data.frame(
        TS_join %>%
        dplyr::group_by(ADM0, ADM_WHO, WHO_cat) %>%
        summarize(S = sum(S, na.rm = TRUE),
                  E = sum(E, na.rm = TRUE),
                  I = sum(I, na.rm = TRUE),
                  Ia = sum(Ia, na.rm = TRUE),
                  R = sum(R, na.rm = TRUE)) %>%
        dplyr::mutate(name = ADM_WHO))
}

#' Join simulation nodes with countries
#'
#' @param outbrk_TS Time-step dataframes for each simulation
#' @param net_pts_join Network point dataframe containing country and WHO information
#' mapped to each node name.
#' 
#' @export
get_outbrks_ctry = function(outbrk_TS, net_pts_join) {
    TS_join = dplyr::left_join(outbrk_TS, net_pts_join, by = c("name" = "cat"))
    
    as.data.frame(
        TS_join %>%
        dplyr::group_by(ADM0) %>%
        summarize(S = sum(S, na.rm = TRUE),
                  E = sum(E, na.rm = TRUE),
                  I = sum(I, na.rm = TRUE),
                  Ia = sum(Ia, na.rm = TRUE),
                  R = sum(R, na.rm = TRUE)) %>%
        dplyr::mutate(name = ADM0))
}
