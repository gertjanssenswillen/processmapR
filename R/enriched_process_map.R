#' @title enrichedProcessMap
#'
#'
#' @description A function for creating a process map of an event log.
#' @param eventlog The event log object for which to create a process map
#' @param aggregationInstructions Is the list of aggregation instructions that will be fired when building the process map.
#'
#' @examples
#' \dontrun{
#' library(eventdataR)
#' data(patients)
#' process_map(patients)
#' }
#' @export enriched_process_map


CreateBaseLog<-function(eventlog){
    tempEventLog <- eventlog %>%
                    as.data.table() %>%
                    droplevels %>%
                    mutate(act = !!activity_id_(eventlog),
                           aid = !!activity_instance_id_(eventlog),
                           case = !!case_id_(eventlog),
                           time = !!timestamp_(eventlog))
        
    base_log <- tempEventLog %>%
                as.data.table() %>%
                group_by(act, aid, case) %>%
                summarize(start_time = min(time),
                          end_time = max(time))
    
    tempEventLogAdditionalAttributes <- tempEventLog[!duplicated(
                                        select(tempEventLog,act,aid,case))
                                        ,]
    
    base_log <-  inner_join(base_log,tempEventLogAdditionalAttributes)
    

    base_log  %>%
        bind_rows(GetEndPoints(baseLog = base_log)) -> base_log
}

GetEndPoints<-function(baseLog){
    baseLog %>%
        group_by(case) %>%
        arrange(start_time) %>%
        slice(c(1,n())) %>%
        mutate(act = c("Start","End")) %>%
        mutate(start_time = recode(act, "End" = end_time, .default = start_time)) %>%
        mutate(end_time = recode(act, "Start" = start_time, .default = end_time))
}

GetBasicNodes <- function(precedence) {
    precedence %>%
        group_by(act, from_id) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(activity_name = 0) %>%
        mutate(shape = if_start_or_end(act,"circle","rectangle"),
               tooltip = act,
               activity_name = if_start_or_end(act, act, tooltip)) %>%
        na.omit()
}

GetBasicEdges <- function(precedence) {
    precedence %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(n = as.double(n())) %>%
        na.omit() %>%
        group_by(act, from_id) %>%
        mutate(label = "") %>%
        ungroup() %>%
        mutate(penwidth = 1,5)
}

edges_performance <- function(precedence, aggregationInstructions, edges) {
    flow_time <- attr(aggregationInstructions, "flow_time")
    temp <- precedence %>%
        ungroup() %>%
        mutate(time = case_when(flow_time == "inter_start_time" ~ as.double(next_start_time - start_time, units = attr(aggregationInstructions, "units")),
                                flow_time == "idle_time" ~ as.double(next_start_time - end_time, units = attr(aggregationInstructions, "units")))) 
    temp <- temp %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(aggr = aggregationInstructions(time)) %>%
        ungroup() %>%
        mutate(tempCol = aggr) %>%
        na.omit() 
    
    temp <- left_join(edges[,c("act", "next_act")], temp) 
    temp <- select(temp, tempCol)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}


edges_frequency <- function(precedence, aggregationInstructions, edges) {
    temp <- precedence %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(tempCol = case_when(aggregationInstructions == "relative" ~ round(100*n/sum(n),2),
                                   aggregationInstructions == "absolute" ~ n)) %>%
        na.omit() 
    temp <- left_join(edges[,c("act", "next_act")], temp) 
    temp <- select(temp, tempCol)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}

edges_columnAgregate <- function(precedence, aggregationInstructions) {
    columnName <-  attr(aggregationInstructions, "columnNameIn")
    columnNamex <- paste0("^",columnName,".x$")
    columnNamey <- paste0("^",columnName,".y$")
    edgeOperation <- attr(aggregationInstructions, "edgeOperation")
    
    columnValues <- precedence %>%
        select(act, aid, columnName) 
    
    p <- precedence %>%
        inner_join(columnValues, by = c( "case" = "case","next_act" = "act", "next_aid" = "aid")) #
    names(p) <- sub(columnNamex, "aggrFirst", names(p))
    names(p) <- sub(columnNamey, "aggrSecond", names(p))
    p$calcColumn <- case_when(
        edgeOperation == "mean" ~  as.double(rowMeans(data.frame(p$aggrFirst,p$aggrSecond))),
        edgeOperation == "min" ~  as.double(do.call(pmin, data.frame(p$aggrFirst,p$aggrSecond))),
        edgeOperation == "max" ~  as.double(do.call(pmax, data.frame(p$aggrFirst,p$aggrSecond))),
        #edgeOperation == "minus" ~  as.double(p$aggrFirst - p$aggrSecond),
        #edgeOperation == "plus" ~ as.double(p$aggrFirst + p$aggrSecond),
        edgeOperation == "from" ~  as.double(p$aggrFirst),
        edgeOperation == "to" ~  as.double(p$aggrSecond),
        TRUE ~  as.double(rowMeans(data.frame(p$aggrFirst,p$aggrSecond)))
    )
    
    temp <- p %>%
        ungroup() %>%
        group_by(act, from_id, next_act, to_id) %>%
        summarize(aggr = aggregationInstructions(calcColumn)) %>%
        ungroup() %>%
        mutate(temp = aggr) %>%
        na.omit() %>%
        select(temp)
    colnames(temp)<-c(attr(aggregationInstructions, "columnNameOut"))
    return(temp)
}

nodes_performance <- function(nodes, precedence, aggregationInstructions) {
    columnName <-  attr(aggregationInstructions, "columnName")
    temp <- precedence %>%
        mutate(duration = as.double(end_time-start_time
                                    , units = attr(aggregationInstructions, "units"))) %>%
        group_by(act, from_id) %>%
        summarize(aggr = aggregationInstructions(duration)) %>%
        ungroup () %>%
        mutate(tempCol = aggr) %>%
        na.omit() %>%
        select(act,tempCol)
    #@nog niet mooi
    temp <- left_join(nodes, temp, by = c("activity_name" = "act")) 
    temp <- select(temp, tempCol)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}

nodes_frequency <- function(nodes, precedence, aggregationInstructions) {
    temp <- precedence %>%
        group_by(act, from_id) %>%
        summarize(n = as.double(n())) %>%
        ungroup() %>%
        mutate(tempCol = case_when(aggregationInstructions == "relative" ~ round(100*n/sum(n),2),
                                   aggregationInstructions == "absolute" ~ n)) %>%
        na.omit() %>%
        select(act,tempCol)
    temp <- left_join(nodes, temp, by = c("activity_name" = "act")) 
    temp <- select(temp, tempCol)
    colnames(temp)<-c(attr(aggregationInstructions, "columnName"))
    return(temp)
}


nodes_columnAgregate <- function(nodes, precedence, aggregationInstructions) {
    names(precedence) <- sub(attr(aggregationInstructions, "columnNameIn"), "aggrCol", names(precedence))
    if (!is.numeric( precedence$aggrCol )){
        stop(paste0("The column: ",attr(aggregationInstructions, "columnNameIn"), " is not numerical."))
    }
    if (sum( !is.na( precedence$aggrCol ) )==0){
        stop(paste0("There seems to be no numerical data in column: ",attr(aggregationInstructions, "columnNameIn")))
    }
    precedence <- precedence[complete.cases(precedence[ ,"aggrCol"]),]
    temp <- precedence %>%
        group_by(act, from_id) %>%
        summarize(aggr = aggregationInstructions(aggrCol)) %>%
        ungroup () %>%
        mutate(tempCol = aggr) %>%
        na.omit() %>%
        select(act,tempCol)
#@nog niet mooi
    temp <- left_join(nodes, temp, by = c("activity_name" = "act")) 
    temp <- select(temp, tempCol)
    colnames(temp)<-c(attr(aggregationInstructions, "columnNameOut"))
    return(temp)
}



if_start_or_end <- function(node, true, false) {
    ifelse(node %in% c("Start","End"), true, false)
}
if_start <- function(node, true, false) {
    ifelse(node %in% c("Start"), true, false)
}

GetBasePrecedence<-function(base_log,eventlog){
    base_log %>%
        ungroup() %>%
        count(act) %>%
        mutate(node_id = 1:n()) -> base_nodes
    
suppressWarnings(base_log %>%
                     ungroup() %>%
                     mutate(act = ordered(act
                                            , levels = c("Start", as.character(activity_labels(eventlog))
                                            , "End"))) %>%
                     group_by(case) %>%
                     arrange(start_time, act) %>%
                     mutate(next_act = lead(act),
                            next_start_time = lead(start_time),
                            next_end_time = lead(end_time),
                            next_aid = lead(aid)) %>%
                     full_join(base_nodes, by = c("act" = "act")) %>%
                     rename(from_id = node_id) %>%
                     full_join(base_nodes, by = c("next_act" = "act")) %>%
                     rename(to_id = node_id) %>%
                     select(-n.x, -n.y))
}

getNodesAggregation <- function(aggregationInstruction,nodes,base_precedence)
{
    perspective <- attr(aggregationInstruction, "perspective")
    if(perspective == "frequency") 
        nodes_frequency(nodes, base_precedence, aggregationInstruction)
    else if(perspective == "performance") 
        nodes_performance(nodes, base_precedence, aggregationInstruction)
    else if(perspective == "columnAgregate") 
        nodes_columnAgregate(nodes, base_precedence, aggregationInstruction)
}

getEdgesAggregation <- function(aggregationInstruction,base_precedence, edges)
{
    perspective <- attr(aggregationInstruction, "perspective")
    if(perspective == "frequency") 
        edges_frequency(base_precedence, aggregationInstruction, edges)
    else if(perspective == "performance")
        edges_performance(base_precedence, aggregationInstruction, edges)
    else if(perspective == "columnAgregate")
        edges_columnAgregate(base_precedence, aggregationInstruction)
}

enriched_process_map <- function(eventlog , aggregationInstructions =  list(frequency("absolute"))) {
        base_log<-CreateBaseLog(eventlog = eventlog)
        base_precedence<- GetBasePrecedence(base_log,eventlog = eventlog)
        nodes<-GetBasicNodes(base_precedence)
        edges<-GetBasicEdges(base_precedence)
    

        create_node_df(n = nrow(nodes),
                       label = nodes$activity_name,
                       shape = nodes$shape,
                       style = "rounded,filled",
                       tooltip = nodes$tooltip,
                       penwidth = 1.5,
                       fixedsize = FALSE,
                       fontname = "Arial") -> nodes_df
        aggregatedColumns<-as.data.table(lapply(aggregationInstructions,getNodesAggregation,nodes,base_precedence))
        #aggregatedColumns$activity_name <- nodes$activity_name
        
        nodes_df <- cbind(nodes_df, aggregatedColumns)
        nodes_df$activity_name <- nodes_df$label
        
        min_level <- min(nodes_df$color_level)
        max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])
        
        create_edge_df(from = edges$from_id,
                       to = edges$to_id,
                       label = edges$label,
                       penwidth = edges$penwidth,
                       fontname = "Arial") -> edges_df
        
        aggregatedEdges<-as.data.table(lapply(aggregationInstructions, getEdgesAggregation, base_precedence, edges))
        edges_df <- cbind(edges_df, aggregatedEdges)
        
        create_graph(nodes_df, edges_df)  %>%
            add_global_graph_attrs(attr = "rankdir", value = "TB",attr_type = "graph") %>%
            add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
            colorize_node_attrs(node_attr_from = "color_level",
                                node_attr_to = "fillcolor",
                                palette = "PuBu",
                                default_color = "white",
                                cut_points = seq(0, 100, length.out = 9))
}

if_end <- function(node, true, false) {
    ifelse(node %in% c("Start","End"), true, false)
}
if_start <- function(node, true, false) {
    ifelse(node %in% c("Start"), true, false)
}

    