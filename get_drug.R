library(httr)
library(data.table)
library(dplyr)


####function####
search_drug_for_gene<- function ( gene_Ensembl = NULL) {
    
    # 设置目标基因的基因ID（雄激素受体 AR 基因的 Ensembl ID）
    # 这里指定了目标基因的唯一标识符，用于后续在查询中使用
    gene_id <- gene_Ensembl
    # gene_id <- "ENSG00000128928"
    #EGFR    ENSG00000146648  >2000 drug
    #CYP11B1 ENSG00000160882  30 drug
    #IVD   ENSG00000128928    0drug   return NULL
    # 构建GraphQL查询字符串，用于获取与目标基因相关的药物信息
    # 这个查询会从OpenTargets平台获取与该基因相关的药物数据
    query_string <- "
      query KnownDrugsQuery(  # GraphQL 查询的名称
        $ensgId: String!  # 参数ensgId：目标基因的Ensembl ID（必需）
        $cursor: String  # 可选的游标参数，用于分页获取数据
        $freeTextQuery: String  # 可选的自由文本查询，用于筛选数据
        $size: Int = 10000  # 默认请求的最大数据量，这里设置为极大的数值，10000已经是极限，也几乎没有药物有大于10000条的数据
      ) {
        target(ensemblId: $ensgId) {  # 查询目标基因信息，使用基因的Ensembl ID
          id  # 返回目标基因的ID
          knownDrugs(cursor: $cursor, freeTextQuery: $freeTextQuery, size: $size) {  # 获取与该基因相关的已知药物信息
            count  # 返回找到的药物数量
            cursor  # 返回下页数据的游标，用于分页查询
            rows {  # 药物的详细信息列表
              phase  # 药物的临床阶段（如临床前、I期、II期等）
              status  # 药物的状态（如是否批准、正在研发等）
              urls {  # 与药物相关的URL链接（可以是相关的研究或网站）
                name  # 链接的名称
                url  # 链接的URL地址
              }
              disease {  # 药物针对的疾病
                id  # 疾病ID
                name  # 疾病名称
              }
              drug {  # 药物的信息
                id  # 药物的ID
                name  # 药物的名称
                mechanismsOfAction {  # 药物的作用机制
                  rows {  # 可能有多个作用机制
                    actionType  # 作用类型（如抑制、激活等）
                    targets {  # 该作用机制的靶标
                      id  # 靶标的ID
                    }
                  }
                }
              }
              drugType  # 药物类型（如小分子、大分子等）
              mechanismOfAction  # 药物的作用机制的概述
            }
          }
        }
      }
    "
    
    # 设置GraphQL API的基础URL
    # 该URL是OpenTargets平台提供的GraphQL查询接口，用于发送请求和接收数据
    base_url <- "https://api.platform.opentargets.org/api/v4/graphql"
    
    # 设置传递给GraphQL API的参数变量对象
    # 变量对象用于在查询时传递参数，确保查询准确
    variables <- list("ensgId" = gene_id)
    
    # 构建POST请求体，包括查询字符串和参数变量
    # POST请求体包含GraphQL查询和要传递的参数（如基因ID），然后以JSON格式发送
    post_body <- list(query = query_string, variables = variables)
    
    # 发送POST请求到API端点
    # 使用POST方法将请求体发送到指定的API端点，获取返回的结果
    r <- POST(url = base_url, body = post_body, encode = 'json')
    
    # 获取API响应的数据内容，并提取返回的数据部分（'data'）
    # 这里的数据包含了药物和与目标基因相关的其他信息
    data <- content(r)$data
    
    # 打印API响应的数据，以便检查返回的结构和内容
    # print(data)
    
    # 提取与目标基因相关的已知药物信息
    # 从响应数据中获取包含药物详细信息的'rows'字段
    known_drugs <- data$target$knownDrugs$rows
    
    # 将提取的药物信息列表转换为数据框格式，便于后续处理
    # 使用lapply函数遍历每个药物，处理每个药物的字段并将其转为数据框的一行
    drugs_df <- do.call(rbind, lapply(known_drugs, function(x) {
      
      # 确保所有字段都存在，避免访问空值导致错误
      # 如果字段缺失（为NULL），则使用NA填充
      drug_id <- ifelse(!is.null(x$drug$id), x$drug$id, NA)
      drug_name <- ifelse(!is.null(x$drug$name), x$drug$name, NA)
      phase <- ifelse(!is.null(x$phase), x$phase, NA)
      status <- ifelse(!is.null(x$status), x$status, NA)
      drug_type <- ifelse(!is.null(x$drugType), x$drugType, NA)
      mechanism_of_action <- ifelse(!is.null(x$mechanismOfAction), x$mechanismOfAction, NA)
      disease_name <- ifelse(!is.null(x$disease$name), x$disease$name, NA)
      disease_id <- ifelse(!is.null(x$disease$id), x$disease$id, NA)
      
      # 处理缺失的URLs字段，拼接成一个字符串
      # 如果存在多个URLs，使用';'分隔，若没有URL，则返回NA
      urls <- ifelse(!is.null(x$urls), paste(sapply(x$urls, function(url) paste(url$name, url$url, sep=": ")), collapse = "; "), NA)
      
      # # 处理缺失的药物作用机制字段
      # # 如果存在多个作用机制，则拼接它们，若没有作用机制，则返回NA
      # action_type <- ifelse(!is.null(x$drug$mechanismsOfAction$rows), 
      #                       paste(sapply(x$drug$mechanismsOfAction$rows, function(moa) moa$actionType), collapse = "; "), NA)
      # 
      # # 处理缺失的靶点信息，若药物有多个靶点，则拼接靶点ID
      # target_ids <- ifelse(!is.null(x$drug$mechanismsOfAction$rows), 
      #                      paste(sapply(x$drug$mechanismsOfAction$rows, function(moa) paste(moa$targets$id, collapse = ",")), collapse = "; "), NA)
      
      action_type_targets <- ifelse(!is.null(x$drug$mechanismsOfAction$rows), 
                                    paste(sapply(x$drug$mechanismsOfAction$rows, function(moa) {
                                      # 检查targets是否为空
                                      if (length(moa$targets) > 0 && !is.null(moa$targets[[1]]$id)) {
                                        # 拼接actionType和靶点id
                                        paste(moa$actionType, "(", paste(moa$targets[[1]]$id, collapse = ","), ")", sep = "")
                                      } else {
                                        # 如果没有靶点信息，返回空字符串
                                        paste(moa$actionType, "(No Target)", sep = "")
                                      }
                                    }), collapse = "; "), NA)
      # 返回格式化后的药物信息为数据框的一行
      # 每个字段在此都经过了缺失值检查，确保数据整洁
      data.frame(
        Drug_id = drug_id,
        Drug_name = drug_name,
        Phase = phase,
        Status = status,
        Drug_type = drug_type,
        Mechanism_of_action = mechanism_of_action,
        Disease_name = disease_name,
        Disease_id = disease_id,
        Urls = urls,
        # action_type = action_type,
        # target_ids = target_ids,
        Action_type_targets = action_type_targets,
        stringsAsFactors = FALSE  # 不将字符串转换为因子
      )
    }))

    return(drugs_df)

    # 打印最终的药物信息数据框，查看数据是否正确
    # 此数据框包含了与目标基因相关的所有药物信息，按列显示
    #head(drugs_df)
}


#####test#######

#EGFR    ENSG00000146648  >2000 drug
#CYP11B1 ENSG00000160882  30 drug
#IVD   ENSG00000128928    0drug

# 
# dataM<-search_drug_for_gene(gene_Ensembl = "ENSG00000128928")
