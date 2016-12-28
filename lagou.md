拉勾网职位数据分析————基于RSelenium的动态网页抓取
================
Ye Ling

摘要
----

最近在找数据分析师方向的工作，因此很想抓取拉钩网上的职位信息加以分析。但由于拉勾网的网页是动态加载的，传统的抓html的办法不适用，试了几次都不成功。幸好有了RSelenium，问题就迎刃而解了。

Selenium是一种浏览器自动化测试框架，功能是用代码遥控浏览器的动作。RSelenium是其在R平台上的实现。有了这个工具，就可以用代码完完全全地模拟人对浏览器的操作了，如打开网页，在搜索框中输入文本，点击确定，点击下一页，输入用户名密码登陆等等。

本文用R基于RSelenium编写了抓取拉勾网招聘信息的爬虫，只需输入职位名称即可下载该职位的所有招聘信息，字段包括职位名称，薪水范围，工作城市，经验要求，学历要求，全职/实习，职业诱惑，职位描述，工作地点，公司名称，公司领域，公司发展阶段，发布时间等。

爬虫程序
--------

以下是爬虫程序。注意使用前需要先启动selenium server。思路是用RSelenium遥控浏览器，打开拉钩主页，搜索指定职位信息，自动翻页，抓取所有招聘职位的网址。这些网址指向的都是静态的html网页，因此接下来只需用rvest包将职位信息一一抓取下来就好了。

``` r
# 输入搜索的职位名，返回所有的职位链接
get_lagou_query_url <- function(keyword) {
  require(RSelenium)
  lagou <- remoteDriver(remoteServerAddr="localhost",
                        port=4444L,                    
                        browserName="firefox"
                        )
  lagou$open()                                                         # 打开浏览器
  cat("firefox opened")
  Sys.sleep(2)
  lagou$navigate("https://www.lagou.com/")                             # 打开拉钩主页
  cat("lagou opened")
  Sys.sleep(2)
  tab_focus <- lagou$findElement(using = "css", value = ".focus")      # 选择城市站
  tab_focus$clickElement()                                             # 选择上海，如此页不跳出则省去这两步
  cat("clicked")
  Sys.sleep(1)
  # 搜索
  query <- lagou$findElement(using = "css", value = "#search_input")    # 找到搜索框
  query$clearElement()                                                  # 清空搜索框，以防有别的文本干扰搜索
  query$sendKeysToElement(list(keyword,key="enter"))                    # 输入搜索关键词，并按下回车         
  cat("query ok")
  Sys.sleep(2)
  
  require(stringr)
  require(magrittr)
  # 获取最大页数
  page_container <- lagou$findElement(using = "css", value = ".pager_container") 
  max_page <- page_container$getElementText() %>% unlist %>%
    str_split(" ") %>% unlist() %>%
    extract(str_detect(., "[0-9]+")) %>%
    as.integer() %>% max()
  cat("there are", max_page, "pages\n")                                      # 向屏幕输出共有多少页
  # 创建url容器
  urls <- c()
  # 获取第一页职位链接
  tmp_url <- lagou$findElements(using = "css selector", value = "li.con_list_item > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > a:nth-child(1)") %>%
    lapply(tmp_url, function(x){x$getElementAttribute("href")}) %>% unlist   # 提取网址
  urls %<>% c(tmp_url)
  cat("get page 1\n")
  # 循环，按max_page-1次next
  for (i in 2:max_page) {                          
    Sys.sleep(2)
    # 找到并点击下一页
    next_page <- lagou$findElement(using = "css", value = ".pager_next")
    next_page$clickElement()
    Sys.sleep(1)                                                             # 此处要停1秒，等页面跳转
    # 获取第i页职位链接
    tmp_url <- lagou$findElements(using = "css selector", value = "li.con_list_item > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > a:nth-child(1)") %>%
      lapply(function(x){x$getElementAttribute("href")}) %>% unlist
    urls %<>% c(tmp_url)
    cat("get page", i, "\n")                                                 # 输出进度
  }
  return(urls)
}
  
#java_url <- get_lagou_query_url("java")
#data_url <- get_lagou_query_url("数据分析师")
#nlp_url <- get_lagou_query_url("自然语言")
#quanzhan_url <- get_lagou_query_url("全栈工程师")

#library(rvest)
#library(tm)
#web <- read_html("https://www.lagou.com/jobs/2692628.html?source=home_hot&i=home_hot-1", encoding = "UTF-8") 
#get_text(web, "h2.fl") %>% stripWhitespace()

# 获取指定css节点的text
get_text <- function(web, css){
  web %>% html_node(css) %>%
    html_text()
}

# 从单个url获取所有职位信息
url2df <- function(url) {
  require(rvest)
  require(tm)
  web <- read_html(url, encoding = "UTF-8")
  pos_name <- get_text(web, "span.name:nth-child(2)")
  salary <- get_text(web, ".salary")
  city <- get_text(web, ".job_request > p:nth-child(1) > span:nth-child(2)")
  work_years <- get_text(web, ".job_request > p:nth-child(1) > span:nth-child(3)")
  degree <- get_text(web, ".job_request > p:nth-child(1) > span:nth-child(4)")
  part_or_full <- get_text(web, ".job_request > p:nth-child(1) > span:nth-child(5)")
  advantage <- get_text(web, ".job-advantage > p:nth-child(2)")
  pos_desc <- get_text(web, ".job_bt > div:nth-child(2)") %>% 
    stripWhitespace()
  work_addr <- get_text(web, ".work_addr") %>%
    stripWhitespace()
  company <- get_text(web, "h2.fl") %>% stripWhitespace()
  hr <- get_text(web, "span.name:nth-child(1)")
  field <- get_text(web, ".c_feature > li:nth-child(1)") %>%
    stripWhitespace()
  phase <- get_text(web, ".c_feature > li:nth-child(2)") %>%
    stripWhitespace()
  #size <- get_text(web, ".c_feature > li:nth-child(3)") %>%
    #stripWhitespace()
  #home_page <- get_text(web, ".c_feature > li:nth-child(4)") %>%
    #stripWhitespace()
  publish_time <- get_text(web, ".publish_time")
  time_stamp <- Sys.Date()
  data.frame(
    pos_name=pos_name,
    salary=salary,
    city=city,
    work_years=work_years,
    degree=degree,
    part_or_full=part_or_full,
    advantage=advantage,
    pos_desc=pos_desc,
    work_addr=work_addr,
    company=company,
    hr=hr,
    field=field,
    phase=phase,
    #size=size,
    #home_page=home_page,
    publish_time=publish_time,
    time_stamp=time_stamp,
    stringsAsFactors = F
  )
}

# 从获取的所有URL获取所有职位信息
get_lagou_pos_df <- function(urls, position) {
  df <- data.frame()
  for (i in 1:length(urls)) {
    Sys.sleep(2)
    temp_df <- url2df(urls[i])
    df %<>% rbind(temp_df)
    if (i%%10==0) {
      cat("successfully get", i, "items of position information\n")                                 # 输出进度，每抓取10条更新一次
      write.csv(df, paste0("E:/QUANT/Selenium/results/item_", i, "_of_", position, ".csv"), row.names = F)
      }
  }
  write.csv(df, paste0("E:/QUANT/Selenium/results/results_of_", position, ".csv"), row.names = F)
  return(df)
}
```

数据分析
--------

总共得到了465条数据（这是拉勾网限制用户能看到的最大条目数）。先看看得到的数据长什么样：

``` r
head(analyst_df)
```

    ##      pos_name   salary    city  work_years       degree part_or_full
    ## 1: 数据分析师 10k-18k  /上海 / 经验1-3年 / 本科及以上 /         全职
    ## 2: 数据分析师 10k-20k  /上海 / 经验1-3年 / 硕士及以上 /         全职
    ## 3: 数据分析师 10k-20k  /上海 / 经验1-3年 / 本科及以上 /         全职
    ## 4: 数据分析师 10k-12k  /上海 / 经验1-3年 / 本科及以上 /         全职
    ## 5: 数据分析师 12k-18k  /上海 / 经验3-5年 / 本科及以上 /         全职
    ## 6: 数据分析师  8k-15k  /上海 /  经验不限 / 硕士及以上 /         全职
    ##                                advantage
    ## 1:  领导nice 扁平管理 补贴 年假 假日福利
    ## 2: 大数据平台,项目管理,培训多多,决策支持
    ## 3: 定期体检 国外outing 弹性工作 绩效奖金
    ## 4:      远程办公 免费午餐晚餐 无加班文化
    ## 5:                     绩效奖金 旅游福利
    ## 6:      C轮,零食福利,发展空间大,弹性工作
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  pos_desc
    ## 1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         -负责萌店用户数据分析，包括用户画像、留存、行为分析，构建有效的分析模型 -负责会员营销系统的人群定义、划分，协助会员营销系统推进精准化营销活动，统计、分析会员营销活动的效果 -参与其他产品的效果分析，协助分析推荐策略效果、用户偏好、产品方向 任职要求： -本科或以上学历 -精通SQL查询 -有hive相关经验优先 -具备良好的业务敏感度 -有用户分析、互联网行业、app产品分析相关经验优先 -一年及以上数据分析经验，掌握用户分析、app分析的方法论，熟悉业务关注点 -具备深入分析问题和解决问题的能力，有强烈的责任心 -具备快速学习能力和高效的执行力 -具备良好的沟通能力 
    ## 2:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      岗位职责：1.能够基于数据分析得到有价值的信息，为业务发展提供策略和建议；2.定期追踪业绩及其他关键业务指标情况，洞察到业务动作，提供决策基础；3.配合公司营运项目需求，及时提交分析报告；4.协助各业务部门完成数据整理、分析与利用，及时提供支持方面的工作； 任职要求：1．互联网相关行业工作经验，对数据化管理有一定的理解；2．良好的数据敏感度与逻辑分析能力，能进行深度的数据信息汇总分析，提炼出有利于公司营运决策的信息；3．熟练操作excel，常规函数、数据透视表、图表制作等，可以用ppt撰写分析报告；4．熟练应用SPSS、Eviews软件或其他统计分析软件；5．了解至少一种数据库软件，能通过sql完成常规取数；6．具有良好的学习、领悟能力，善于沟通协调。 
    ## 3:  Responsibilities: 1. Develop smart, integrated, research-based proposals and presentations to enhance Zamplus's reputation with clients as a consultant and partner. 2. Acquire and share deep knowledge of a specific industry sector, its competitive landscape, client products, and key business issues that affect the client and industry. 3. Maintain monthly industry and client dashboards on Global performance by industry, and lead strategic analysis projects. 4. Analyze industry and sub-vertical trends using third-party research and portfolio of Zamplus proprietary tools, and translate analyses into actionable, prioritized plans for teams to implement with clients. 5. Create and standardize a methodology and proces that capture's a clients set of business goals, the reasons why they are believed attainable, and the plan for reaching those goals. Requirements: 1. Bachelor's degree preferred with a strong academic record. 2. At least 3 years of sales support and market research experience. 3. Superior analytical skills that include the ability to see granular as well as big-picture issues. 4. Strong project management, interpersonal, and organizational skills. 5. Advanced level of proficiency in Microsoft Excel and PowerPoint. This position is based in Shanghai, China. 
    ## 4:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               工作职责： 归属体验闭环组，主要负责如下事项: 制定数据采集规范并维护知识库 支持所负责领域业务团队的数据需求，参与制定关键指标，制作报表并指导解读 基于数据分析与探查，帮助衡量产品表现 发起并带领工程师进行必要的基础设施的迭代，完成新增数据采集与指标呈现的需求 有机会根据分析结论负责特定的产品模块的迭代 加分项 有优秀的项目分析经验 熟悉 SQL、NoSQL 等查询语言 熟悉主要的BI 工具 对协作类产品有了解 
    ## 5:                                                                                                                                                                               岗位职责 1.负责贷记卡产品的信用和欺诈风险分析； 2.针对贷记卡领域的征信评级、调额流程环节等场景，设计模型体系并进行数量建模工作； 3.开发贷记卡的申请评分卡、行为评分卡和催收评分卡和反欺诈评分卡等模型； 4.依据风险特征和模型评分结果设置相应的风控策略，对风险进行控制 5.根据风险特征，设计模型开发需求以及模型在策略中的应用方案 6.设计和完善风险监控报表体系，并定期输出数据分析报告 7.对公司的交叉销售、风险定价、客户挽留、市场营销策略等重点项目提供支持； 8.搜集行业研究报告及相关企业年报，对指标性企业进行数据监控； 9.协助技术人员进行数据仓库的建设和应用工作，以及数据分析模型搭建工作； 10.定期制作关于信贷业务的专题分析； 任职要求 1.本科及以上学历，计算机、金融、统计、计量经济学、数学、数据挖掘等拥有定量分析背景的专业优先； 2.3年以上数据分析、建模或风险运营的工作经验，拥有金融服务业/信用卡/小微企业贷款等行业背景为佳； 3.熟悉贷记卡类产品的信用与欺诈风险的管理方法，同时对互联网信贷产品有基本了解优先； 4.对数据敏感，逻辑分析能力强，能拆分并解决问题，有产品、运营和风控工作背景优先； 5.熟悉掌握常用建模工具如线性回归，逻辑回归，决策树，聚类分析，神经网络等； 积极上进，有责任心，有较强的学习能力和良好的团队合作精神 
    ## 6:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                职位描述： 1，负责业务数据的日常分析，制定评估指标体系，协助各具体产品进行自身的分析； 2，负责竞品数据监控等； 3，深入分析业务数据变动的内在原因，给业务和开发指明正确的解决方向； 4，快速支撑日常临时需求分析任务。 职位要求 1，本科以上学历 2，熟悉数据分析、数据挖掘专业知识，具备数据分析、数据挖掘的能力； 3，熟悉Office工具（Excel、PPT为主），擅长撰写分析报告； 4，掌握sql、hive等技术并熟练应用； 5，熟悉R、SPSS、SAS等统计分析软件至少一种； 6，2年以上互联网数据分析经验 ，电商行业尤佳。 7，逻辑思维能力强，沟通表达优秀，细心谨慎，合作精神强，抗压能力强 
    ##                                                                        work_addr
    ## 1:                  上海 - 宝山区 - 淞南 - 长江路 258 号 B1 栋微盟大厦 查看地图 
    ## 2:                       上海 - 静安区 - 南京西路699号东方有线大厦30楼 查看地图 
    ## 3:                     上海 - 闸北区 - 汶水路 - 灵石路695号3号楼1101室 查看地图 
    ## 4:                        上海 - 浦东新区 - 碧波路888号畅星大厦圆楼2楼 查看地图 
    ## 5:                        上海 - 长宁区 - 北新泾 - 金钟路968号SOHO大楼 查看地图 
    ## 6:  上海 - 浦东新区 - 上海市浦东新区峨山路91弄陆家嘴软件园9号楼北楼8楼 查看地图 
    ##                            company      hr                      field
    ## 1:              微盟 拉勾认证企业       cv  移动互联网,电子商务 领域 
    ## 2:  上海链家(原德佑) 拉勾认证企业    mandy             企业服务 领域 
    ## 3:   晶赞科技Zamplus 拉勾认证企业  Zamplus    数据服务,文化娱乐 领域 
    ## 4:        Teambition 拉勾认证企业   Evelyn  移动互联网,企业服务 领域 
    ## 5:            携程 拉勾未认证企业  yf.yang      移动互联网,旅游 领域 
    ## 6:            买单侠 拉勾认证企业   Sherry                 金融 领域 
    ##                  phase                    publish_time time_stamp
    ## 1:       C轮 发展阶段       13:22<U+00A0> 发布于拉勾网 2016-12-27
    ## 2:       B轮 发展阶段       16:59<U+00A0> 发布于拉勾网 2016-12-27
    ## 3:       C轮 发展阶段       1天前<U+00A0> 发布于拉勾网 2016-12-27
    ## 4:       B轮 发展阶段       2天前<U+00A0> 发布于拉勾网 2016-12-27
    ## 5:  上市公司 发展阶段  2016-12-20<U+00A0> 发布于拉勾网 2016-12-27
    ## 6:       C轮 发展阶段       11:00<U+00A0> 发布于拉勾网 2016-12-27

### 职位分布地图

首先感兴趣的是这些职位分布在全上海的那些位置。地理信息作图有一个很好用的包是REmap，其原理是调用百度地图的api接口，将地址信息通过百度搜索转化为经纬度，然后在百度地图上显示。

第一步是处理地址信息：

``` r
library(stringr)
library(tm)
addrs <- analyst_df$work_addr %>%
  str_replace_all("查看地图", "") %>%
  removePunctuation() %>%
  stripWhitespace() %>%
  str_trim()
head(addrs)
```

    ## [1] "上海 宝山区 淞南 长江路 258 号 B1 栋微盟大厦"                  
    ## [2] "上海 静安区 南京西路699号东方有线大厦30楼"                     
    ## [3] "上海 闸北区 汶水路 灵石路695号3号楼1101室"                     
    ## [4] "上海 浦东新区 碧波路888号畅星大厦圆楼2楼"                      
    ## [5] "上海 长宁区 北新泾 金钟路968号SOHO大楼"                        
    ## [6] "上海 浦东新区 上海市浦东新区峨山路91弄陆家嘴软件园9号楼北楼8楼"

然后将地址转化为经纬度信息：

``` r
library(REmap)
options(digits = 7)                         # 7位有效数字，确保经纬度的区分度
addr_geo <- get_geo_position(addrs) %>%
  as.data.table()
head(addr_geo)
```

    ##         lon      lat
    ## 1: 121.5025 31.35670
    ## 2: 121.4698 31.23655
    ## 3: 121.4551 31.28980
    ## 4: 121.5903 31.20359
    ## 5: 121.5405 31.22138
    ## 6: 121.6098 31.20581
    ##                                                              city
    ## 1:                   上海 宝山区 淞南 长江路 258 号 B1 栋微盟大厦
    ## 2:                      上海 静安区 南京西路699号东方有线大厦30楼
    ## 3:                      上海 闸北区 汶水路 灵石路695号3号楼1101室
    ## 4:                       上海 浦东新区 碧波路888号畅星大厦圆楼2楼
    ## 5: 上海 浦东新区 上海市浦东新区峨山路91弄陆家嘴软件园9号楼北楼8楼
    ## 6:               上海 浦东新区 张江 张江高科软件园博霞路50号402室

作图函数如下，生成的是动态html页面，这里展示的是网页截图：

``` r
#remapB(markPointData = data.frame(addr_geo$city),                             # 所有的地址
#       markPointTheme = markPointControl(symbol = "pin", color = "red",       # symbol符号形状，color符号颜色
#                                         effect = F, symbolSize = 2),         # effect动态效果，symbosize符号大小
#       geoData = addr_geo,                                                    # 包括三列，经度，纬度和地址，注意经纬度的有效数字要7位
#       center = get_city_coord("shanghai"),                                   # 图片中心
#       zoom=12,                                                               # 图片缩放大小，城市用15，国家用5
#       title = "上海数据分析师职位分布图",
#       subtitle = "by Ye Ling"
#)
```

从图中可以看到用人单位主要集中在市区，其中有三个相对集中的区域，分别是徐汇区的漕河泾开发区，浦东的张江地区和世纪大道-浦建路周边。郊区的嘉定，青浦，松江闵行南部和宝山北部机会极少。此外，企业沿着地铁沿线分布的趋势非常明显。

### Python or R or SPSS or SAS?

目前流行的用于数据分析的语言中，开源的有Python和R，收费的有SPSS和SAS。那么招聘企业偏好应聘者会使用哪种语言呢？

首先需要对职位描述（pos\_desc）字段做清洗，分词处理。这里用到的函数与“知乎用户对特朗普和希拉里的看法——基于爬虫和文本挖掘技术的分析”大同小异，就不赘述了。由于有的职位描述是中英文混杂的，有的是纯英文，且一些英文单词对分析很重要，因此对所有英文单词都予以保留。来看看清理后的效果：（为了便于分析，所有英文单词都转为大写）

``` r
head(analyst_vec)
```

    ## [1] "店 用户  用户 画像 行为 分析 有效的 分析模型 会员 营销 系统 人群 定义 会员 营销 系统 精准 营销 活动 分析 会员 营销 活动 效果 产品 效果 分析 分析 策略 效果 用户 偏好 产品  本科 学历 SQL HIVE 经验 业务 敏感度 用户 分析 互联网 行业 APP 产品 分析 经验  经验 用户 分析 APP 分析的 方法论 业务 分析 问题 问题 能力 责任心 学习能力 力 能力"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    ## [2] "岗位 职责  价值 信息 业务 发展 策略 建议 业绩 关键 业务 指标 情况 业务 动作 决策 基础 公司 营运 项目 需求 分析 报告 业务 部门 数据 分析  工作 互联网 行业 工作 经验 数据 管理 数据 敏感度 逻辑分析 能力 深度 数据 信息 分析 公司 营运 决策 信息 EXCEL 常规 函数 数据 表 图表 PPT 分析 报告 应用 SPSS EVIEWS 软件 统计分析 软件 数据库 软件 SQL 常规 能力"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    ## [3] "RESPONSIBILITIES DEVELOP SMART INTEGRATED RESEARCH BASED PROPOSALS  PRESENTATIONS  ENHANCE ZAMPLUS S REPUTATION  CLIENTS   CONSULTANT  PARTNER ACQUIRE  SHARE DEEP KNOWLEDGE   SPECIFIC INDUSTRY SECTOR  COMPETITIVE LANDSCAPE CLIENT PRODUCTS  KEY BUSINESS ISSUES  AFFECT  CLIENT  INDUSTRY MAINTAIN MONTHLY INDUSTRY  CLIENT DASHBOARDS  GLOBAL PERFORMANCE  INDUSTRY  LEAD STRATEGIC ANALYSIS PROJECTS ANALYZE INDUSTRY  SUB VERTICAL TRENDS USING THIRD PARTY RESEARCH  PORTFOLIO  ZAMPLUS PROPRIETARY TOOLS  TRANSLATE ANALYSES  ACTIONABLE PRIORITIZED PLANS  TEAMS  IMPLEMENT  CLIENTS CREATE  STANDARDIZE  METHODOLOGY  PROCES  CAPTURE S  CLIENTS SET  BUSINESS GOALS  REASONS    BELIEVED ATTAINABLE   PLAN  REACHING  GOALS REQUIREMENTS BACHELOR S DEGREE PREFERRED   STRONG ACADEMIC RECORD AT LEAST YEARS  SALES SUPPORT  MARKET RESEARCH EXPERIENCE SUPERIOR ANALYTICAL SKILLS  INCLUDE  ABILITY  SEE GRANULAR  WELL  BIG PICTURE ISSUES STRONG PROJECT MANAGEMENT INTERPERSONAL  ORGANIZATIONAL SKILLS ADVANCED LEVEL  PROFICIENCY  MICROSOFT EXCEL  POWERPOINT THIS POSITION  BASED  SHANGHAI CHINA"
    ## [4] "工作 职责 归属 体验 闭环 事项 数据采集 知识库 领域 业务 团队 数据 需求 关键 指标 报表 指导  产品 工程师 基础设施 迭代 数据采集 指标 需求 机会 分析 结论 产品 模块 迭代 项目分析 经验 SQL NOSQL 查询语言 BI 工具 产品"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
    ## [5] "岗位 职责 卡 产品 信用 欺诈 风险分析 卡 领域 信 评级 额 流程 环节 场景 设计 模型 体系 数量 建模 工作 卡 卡 行为 卡 催收 卡 欺诈 卡 模型 依据 风险 特征 模型  风 策略 风险 风险 特征 设计 模型 需求 模型 策略 应用 方案 设计 风险 监控 报表 体系  报告 公司 交叉 销售 风险 定价 客户 市场 营销 策略 重点 项目 行业 研究 报告 企业 年报 指标 性 企业 数据 监控 技术人员 数据仓库 建设 应用 工作 数据 分析模型 工作 信贷 业务 专题 分析 本科 学历 计算机 金融 计量经济学 数学 数据挖掘 背景 专业  建模 风险 工作 经验 金融 服务业 信用卡 企业 贷款 行业 背景 为佳 卡 产品 信用 欺诈 风险 管理 方法 互联网 信贷 产品 数据 逻辑分析 能力 拆分 问题 产品 运营 和风 工作 背景 建模 工具 线性回归 逻辑 决策树 聚类分析 神经网络 责任心 学习能力 团队 合作 精神"                                                                                                                                                                                                                                                                                                                                                               
    ## [6] "职位 业务 数据 分析 评估 指标 体系 产品 分析 监控 分析 业务 数据 变动 内在 原因 业务 正确的  需求分析 任务 职位 本科 学历  数据挖掘 专业知识  数据挖掘 能力 OFFICE 工具 EXCEL PPT 分析 报告 SQL HIVE 技术 应用 R SPSS SAS 统计分析 软件 互联网  经验 电 商 行业 能力 合作 精神 能力"

接下来就可以看四种语言受企业的欢迎程度了：

``` r
# 有多少企业要求R  
analyst_vec[str_detect(analyst_vec, "\\bR\\b")] %>% length()
```

    ## [1] 118

``` r
# 有多少企业要求python  
analyst_vec[str_detect(analyst_vec, "PYTHON")] %>% length()
```

    ## [1] 136

``` r
# 有多少企业要求SPSS  
analyst_vec[str_detect(analyst_vec, "SPSS")] %>% length()
```

    ## [1] 91

``` r
# 有多少企业要求SAS  
analyst_vec[str_detect(analyst_vec, "\\bSAS\\b")] %>% length()
```

    ## [1] 93

Python在所有465家企业中得136票，占比136/465=29.2%，排名第一。R得118票（25.4%）为第二名，第三名是SAS（20%），第四名是SPSS（19.6%）。注意企业可以对语言做多种要求，也可以不做要求，所以上述百分比之和不会是100%。

显然开源免费的软件更受欢迎。那么单独要求Python或R，以及Python或R都可接受的企业有多少呢？

从下图可以看到有73家企业只接受Python，55家企业只接受R，另有63家企业两种语言都可以接受。总的来说，Python的受接受度更高一些。

![](lagou_files/figure-markdown_github/unnamed-chunk-12-1.png)

### 薪资水平

薪资水平和工作经验高度相关，因此分成四组：无经验（包括应届），1-3年，3-5年，和5-10年。先看看这些分组的分布如何：

``` r
table(work_years)
```

    ## work_years
    ##      1-3年   10年以上    1年以下      3-5年     5-10年       不限 
    ##        164          1          3        187         52         47 
    ## 应届毕业生 
    ##         11

要求经验在3-5年的公司最多，187家；其次是1-3年，164家，不限或应届或1年以下加起来61家，5-10年52家，要求十年以上的仅一家。

下图展示了以工作经验分组的平均最低薪水和最高薪水。可以看到，保守而言，最低起薪在7k元左右，工作3-5年后可以翻一倍到14k元左右，5-10年后可达21k。

``` r
salary_dt[, .(mean_min_sal=mean(min_sal), mean_max_slary=mean(max_sal), count=.N), by=work_years][order(mean_min_sal)]
```

    ##    work_years mean_min_sal mean_max_slary count
    ## 1:     无经验         7459          12869    61
    ## 2:      1-3年        10128          17518   164
    ## 3:      3-5年        14171          23882   187
    ## 4:     5-10年        21075          34094    53

### 职位描述

下面是数据分析师职位描述中最常出现的20个词。需要说明的是"数据分析"一词已被过滤，"R"由于是单字母，生成tdm时被程序自动过滤了。

    ##         word freq
    ##  1:   大数据  306
    ##  2:      sql  275
    ##  3:   数据库  270
    ##  4: 数据挖掘  251
    ##  5:   hadoop  242
    ##  6:   计算机  217
    ##  7:   互联网  200
    ##  8: 数据仓库  176
    ##  9:     hive  167
    ## 10:    spark  161
    ## 11:   python  140
    ## 12:     java  130
    ## 13:    mysql  112
    ## 14:    hbase  111
    ## 15:    excel  107
    ## 16:   统计学  103
    ## 17:      etl   97
    ## 18:      sas   96
    ## 19:    linux   92
    ## 20: 机器学习   91

绘成词云：

这些词充分展示了一个数据分析师需要掌握的技能和素质。

### 企业发展阶段

从下图可以看到，各个发展阶段的企业都有，没有十分集中的趋势，从B轮到D轮公司数量逐渐减少。 ![](lagou_files/figure-markdown_github/unnamed-chunk-19-1.png)

### 总结

本文用R编写了一个基于RSelenium的爬虫，用于爬取拉勾网上任意职位的所有信息。基于获得的关于数据分析师的信息，本文绘制了职位分布地图，分析了那种数据分析语言最受欢迎，数据分析行业的薪资水平，数据分析师应该具备的技能和素质以及招聘企业的发展阶段。
