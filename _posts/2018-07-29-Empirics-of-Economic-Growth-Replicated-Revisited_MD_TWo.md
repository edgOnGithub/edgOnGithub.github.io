---
layout: post
author: "Ed"
title: "Empirics MD Two"
---




In this post I replicate the findings of Mankiw, Romer and Weil's 'A Contribution to the Empirics of Economic Growth'. This _should_ be a pretty simple task since the paper only uses 121 observations; has been replicated before by Bernanke and Giirkaynak and uses a straightforward econometric framework but I run into a few issues. Ultimately, I successfully replicate the authors' main findings but fail to satisfactorily replicate the authors' estimates of the Solow residual (i.e. we get different intercepts). The 'new' old data I collect is available on GitHub. 

## Data

MRW use data from the Real National Accounts collated by Summers and Heston - now known as the Penn World Table - as well as the World Bank's _World Tables_  and _World Development Report_ 1988.^[Ben S. Bernanke & Refet S. Gürkaynak, 2002. "Is Growth Exogenous? Taking Mankiw, Romer, and Weil Seriously," NBER Chapters,in: NBER Macroeconomics Annual 2001, Volume 16, pages 11-72 National Bureau of Economic Research, Inc.] Unfortunately, the PWT 4.0, which MRW use, is no longer available online (the earliest version available is 5.6 but this has revised figures) however the advantage of replicating a paper written in 1992 is that all the data can be found tabulated in the appendix:

![]({{ "/assets/appendix_subset.png" | absolute_url }})

Converting the data from an appendix pdf table into R requires the tabulizer library (and its dependency rJava):

```r
library(tabulizer)
library(dplyr)


MRW_pdf <- extract_tables('Data/Original Data/MRW_appendix.pdf')

# Each list entry corresponds to a page of the appendix
df.1 <- MRW_pdf[[1]] %>% 
  as_tibble()

df.2 <- MRW_pdf[[2]] %>% 
  as_tibble()

df.3 <- MRW_pdf[[3]] %>% 
  as_tibble()
knitr::kable(head(df.1), format =  'markdown')
```



|V1     |V2           |V3 |V4 |V5 |V6   |V7   |V8  |V9      |V10  |V11 |
|:------|:------------|:--|:--|:--|:----|:----|:---|:-------|:----|:---|
|Number |Country      |N  |I  |0  |1960 |1985 |GDP |age pop |     |    |
|1      |Algeria      |1  |1  |0  |2485 |4371 |4.8 |2.6     |24.1 |4.5 |
|2      |Angola       |1  |0  |0  |1588 |1171 |0.8 |2.1     |5.8  |1.8 |
|3      |Benin        |1  |0  |0  |1116 |1071 |2.2 |2.4     |10.8 |1.8 |
|4      |Botswana     |1  |1  |0  |959  |3671 |8.6 |3.2     |28.3 |2.9 |
|5      |Burkina Faso |1  |0  |0  |529  |857  |2.9 |0.9     |12.7 |0.4 |

Unfortunately not everything is perfect: the variables are saved as strings, the column names appear as the first row and some values are in the columns they shouldn't be; here the OECD dummy and GDP in 1960 have both been stored in V5:


```r
print(class(df.3$V6))
```

```
[1] "character"
```

```r
knitr::kable(df.3 %>% filter(V2 == 'United States'))
```



V1    V2              V3   V4   V5         V6   V7       V8    V9    V10    V11  
----  --------------  ---  ---  ---------  ---  -------  ----  ----  -----  -----
104   United States   1    1    1 12,362        18,988   3.2   1.5   21.1   11.9 

The column names are pretty easy to fix using a generic cleaning function and mapping the three appendix page dataframes into a single tibble: 

```r
library(purrr)

clean.table <- function(table, columns){
  table <- table[2:nrow(table), ]
  colnames(table) <- columns
  
  return(table)
}
cols <- c("number",
          "country",
          "N",
          "I",
          "O",
          "1960",
          "1985",
          "gdp_ch",
          "working_age_pop_ch",
          "s",
          "school")

MRW_data <- list(df.1, df.2, df.3) %>% 
  map_df(~clean.table(table = .x, columns = cols)) %>% 
  mutate(nchar_oecd = nchar(O))
```

The jumbled values are a bit harder to solve. Spotting that errors aren't random but only occur in observations with 5 digit (i.e. 10,000+) GDP figures speeds up the process - this is where `nchar_oecd` above comes in. We can use `separate` to split the OECD column whenever it encounters a space character.

```r
library(tidyr)
MRW_data_ok_oecd <- MRW_data %>% 
  filter(nchar_oecd == 1) %>% 
  select(-nchar_oecd)


MRW_data_not_ok_oecd <- MRW_data %>% 
  filter(nchar_oecd > 1) %>% 
  separate(col = 'O', sep = ' ', into = c('O', '1960')) %>% 
  select(-nchar_oecd)
```




 Next we convert the strings to numeric values whilst taking into account the presence of commas in the 1960 and 1985 GDP variables. Finally, we add 5 to the `working_age_pop_ch` variable - this corresponds to MRW using 0.05 as the value of depreciation and technology growth.^[Mankiw, Romer and Weil aren't entirely consistent in their treatment of percentages in the paper - the appendix shows that working age population change has mean value 2.28 but the authors refer to $g + d = 0.05$ in the main body of the paper. Common sense and footnote 6 suggest that this means a 5% depreciation rate and the mean working age population should be treated as 2.28\% (and not 200\%...), I return to this later when the Solow residual fails to match.]

```r
MRW_clean <- bind_rows(MRW_data_not_ok_oecd,
                       MRW_data_ok_oecd) %>%
  mutate_at(c('N',
              'number',
              'I',
              'O',
              'gdp_ch',
              'working_age_pop_ch',
              's',
              'school'), as.numeric) %>% 
  mutate('1985' = as.numeric(gsub(',', '', `1985`)),
         '1960' = as.numeric(gsub(',', '', `1960`))) %>% 
  mutate(n_g_d = working_age_pop_ch + 5) 
```

The data has the right number of OECD, intermediate and non-oil countries and no particularly unusual observations suggesting anything has gone awry. I have checked every OECD datapoint against the original appendix and a (ad-hoc) random sample of non-OECD observations. (Missing values come from those missing in the original appendix.)

```r
library(plotly)
library(ggplot2)

skimr::skim(MRW_clean) # skimr's histograms are known to be buggy in Rmd.
```

```
Skim summary statistics
 n obs: 121 
 n variables: 12 

Variable type: character 
 variable missing complete   n min max empty n_unique
  country       0      121 121   4  18     0      121

Variable type: numeric 
           variable missing complete   n    mean      sd    p0     p25
               1960       5      116 121 3681.82 7492.88 383    973.25
               1985      13      108 121 5683.26 5688.67 412   1209.25
             gdp_ch       4      117 121    4.09    1.89  -0.9    2.8 
                  I       0      121 121    0.62    0.49   0      0   
                  N       0      121 121    0.81    0.39   0      1   
              n_g_d      14      107 121    7.28    1      5.3    6.7 
             number       0      121 121   61      35.07   1     31   
                  O       0      121 121    0.18    0.39   0      0   
                  s       0      121 121   18.16    7.85   4.1   12   
             school       3      118 121    5.53    3.53   0.4    2.4 
 working_age_pop_ch      14      107 121    2.28    1      0.3    1.7 
     p50     p75    p100     hist
 1962    4274.5  77881   <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
 3484.5  7718.75 25635   <U+2587><U+2583><U+2581><U+2581><U+2582><U+2581><U+2581><U+2581>
    3.9     5.3      9.2 <U+2581><U+2582><U+2585><U+2587><U+2586><U+2583><U+2582><U+2581>
    1       1        1   <U+2585><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587>
    1       1        1   <U+2582><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587>
    7.4     7.9     11.8 <U+2583><U+2583><U+2587><U+2586><U+2581><U+2581><U+2581><U+2581>
   61      91      121   <U+2587><U+2587><U+2587><U+2587><U+2587><U+2587><U+2587><U+2587>
    0       0        1   <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2582>
   17.7    24.1     36.9 <U+2586><U+2586><U+2587><U+2587><U+2587><U+2585><U+2585><U+2581>
    4.95    8.17    12.1 <U+2587><U+2587><U+2586><U+2583><U+2585><U+2586><U+2583><U+2585>
    2.4     2.9      6.8 <U+2583><U+2583><U+2587><U+2586><U+2581><U+2581><U+2581><U+2581>
```

A few interactive plots using `plotly`:


```r
MRW_clean_subset <- MRW_clean %>% 
  select(-O)
q <- ggplot(MRW_clean, aes(y = log(`1960`), x = school, text = country)) +
  geom_point(data = MRW_clean_subset, colour = 'grey', alpha = 0.2, fill = 'grey') +
  geom_point(data = MRW_clean, colour = 'deeppink') +
  facet_wrap(~O) +
  ylab('Log Real GDP pc in 1960') +
  ggtitle('Schooling vs GDP in OECD and non-OECD countries') +
  theme_minimal()

rm(MRW_clean_subset)
ggplotly(q)
```

<!--html_preserve--><div id="222d072cb1527" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="222d072cb1527">{"x":{"data":[{"x":[5.8,8,7.9,4.8,5.5,8.9,12.1,10.6,7,5.8,3.9,2.4,1.9,3.7,11.2,6.6,5.8,11.6,8.8,11.9,5,4.9,4.7,7.7,6.1,7.2,11.7,4.4,8,8.1,7,7,9.8,8.1,4.1,11.9,1.5,4.5,1.8,1.8,2.9,0.4,0.4,3.4,1.4,0.4,3.8,7,1.1,2.6,1.5,4.7,null,2.3,2.4,2,2.5,2.6,0.6,1,1,7.3,3.6,0.7,0.5,2.3,0.4,1.7,1.7,1.1,3,2,3.7,0.5,2.9,4.3,1.1,3.6,2.4,4.4,0.9,12.1,3.2,3.5,7.2,5.1,6.5,7.4,9.5,10.9,10.8,10.2,9.6,7.3,2.3,2.7,3,10.6,3.1,9,8.3,8.8,null,4.4,null,0.6,8,9.3,8.2,10.7,11.5,8.9,8.4,7.9,10.2,11.4,7.1,5,7.1,10.7,10],"y":[7.72841577984104,8.2337687092171,8.9621353900666,9.24067557177226,7.72929567431048,8.94036723330517,8.05990833457828,9.23853902633486,8.11969625295725,7.56992765524265,7.62168499872461,7.8164169836918,6.99942246750796,7.26542972325395,7.91059061225648,8.34972083747249,8.06934236681164,7.79276172081653,9.13270310224903,9.42238253021922,8.48714627006394,7.38894609761844,7.51860721681525,8.55429627936774,7.89058253465654,7.69530313496357,7.92334821193015,7.57609734062311,8.10470346837111,8.07899825868515,8.54071438645758,9.24638296332311,9.04073758759,8.19808924895612,6.77878489768518,9.16146520419405,7.48493028328966,7.81802793853073,7.37023064180708,7.01750614294126,6.86589107488344,6.2709884318583,6.62671774924902,6.7900972355139,6.73101810048208,6.81124437860129,6.91671502035361,6.81014245011514,6.27852142416584,7.17548971362422,6.68336094576627,6.91671502035361,6.61472560020376,7.23417717974985,6.8501261661455,6.06610809010375,6.76041469108343,7.08506429395255,6.12029741895095,6.60258789218934,6.65544035036765,7.58731050602262,6.93731408122368,7.25841215059531,6.289715570909,6.96129604591017,6.13122648948314,7.23849684089437,6.2363695902037,6.80350525760834,8.46968220874519,7.13409372119287,6.70563909486,5.94803498918065,6.65544035036765,7.39203156751459,6.39859493453521,6.38687931936265,7.25134498337221,7.07918439460967,7.10987946307227,null,6.74051935960622,6.24804287450843,8.03430693633949,6.88550967003482,8.19035440376326,8.50025047068593,8.4767877767812,8.15851624480683,7.68845535654994,7.15851399732932,11.2629372996708,7.67508185771633,6.72503364216684,null,6.98193467715639,7.41938058291869,8.81447900001071,7.93487156594518,7.49220304261874,7.77569574991525,null,7.17625453201714,null,null,8.68929604801586,8.82305893430165,7.98888225330923,9.05380351415596,8.78370269863522,8.88391747120797,8.94832604627298,7.72179177681754,8.99850761180784,8.39185670010494,8.49964003216865,9.10664513563742,7.7376162828579,8.94754601503218,8.97941663334301],"text":["~school:  5.8<br />~log(`1960`):  7.728416<br />Portugal","~school:  8.0<br />~log(`1960`):  8.233769<br />Spain","~school:  7.9<br />~log(`1960`):  8.962135<br />Sweden","~school:  4.8<br />~log(`1960`):  9.240676<br />Switzerland","~school:  5.5<br />~log(`1960`):  7.729296<br />Turkey","~school:  8.9<br />~log(`1960`):  8.940367<br />United Kingdom","~school: 12.1<br />~log(`1960`):  8.059908<br />Barbados","~school: 10.6<br />~log(`1960`):  9.238539<br />Canada","~school:  7.0<br />~log(`1960`):  8.119696<br />Costa Rica","~school:  5.8<br />~log(`1960`):  7.569928<br />Dominican Rep.","~school:  3.9<br />~log(`1960`):  7.621685<br />El Salvador","~school:  2.4<br />~log(`1960`):  7.816417<br />Guatemala","~school:  1.9<br />~log(`1960`):  6.999422<br />Haiti","~school:  3.7<br />~log(`1960`):  7.265430<br />Honduras","~school: 11.2<br />~log(`1960`):  7.910591<br />Jamaica","~school:  6.6<br />~log(`1960`):  8.349721<br />Mexico","~school:  5.8<br />~log(`1960`):  8.069342<br />Nicaragua","~school: 11.6<br />~log(`1960`):  7.792762<br />Panama","~school:  8.8<br />~log(`1960`):  9.132703<br />Trinidad & Tobago","~school: 11.9<br />~log(`1960`):  9.422383<br />United States","~school:  5.0<br />~log(`1960`):  8.487146<br />Argentina","~school:  4.9<br />~log(`1960`):  7.388946<br />Bolivia","~school:  4.7<br />~log(`1960`):  7.518607<br />Brazil","~school:  7.7<br />~log(`1960`):  8.554296<br />Chile","~school:  6.1<br />~log(`1960`):  7.890583<br />Colombia","~school:  7.2<br />~log(`1960`):  7.695303<br />Ecuador","~school: 11.7<br />~log(`1960`):  7.923348<br />Guyana","~school:  4.4<br />~log(`1960`):  7.576097<br />Paraguay","~school:  8.0<br />~log(`1960`):  8.104703<br />Peru","~school:  8.1<br />~log(`1960`):  8.078998<br />Surinam","~school:  7.0<br />~log(`1960`):  8.540714<br />Uruguay","~school:  7.0<br />~log(`1960`):  9.246383<br />Venezuela","~school:  9.8<br />~log(`1960`):  9.040738<br />Australia","~school:  8.1<br />~log(`1960`):  8.198089<br />Fiji","~school:  4.1<br />~log(`1960`):  6.778785<br />Indonesia","~school: 11.9<br />~log(`1960`):  9.161465<br />New Zealand","~school:  1.5<br />~log(`1960`):  7.484930<br />Papua New Guinea","~school:  4.5<br />~log(`1960`):  7.818028<br />Algeria","~school:  1.8<br />~log(`1960`):  7.370231<br />Angola","~school:  1.8<br />~log(`1960`):  7.017506<br />Benin","~school:  2.9<br />~log(`1960`):  6.865891<br />Botswana","~school:  0.4<br />~log(`1960`):  6.270988<br />Burkina Faso","~school:  0.4<br />~log(`1960`):  6.626718<br />Burundi","~school:  3.4<br />~log(`1960`):  6.790097<br />Cameroon","~school:  1.4<br />~log(`1960`):  6.731018<br />CentralAfr. Rep.","~school:  0.4<br />~log(`1960`):  6.811244<br />Chad","~school:  3.8<br />~log(`1960`):  6.916715<br />Congo, Peop. Rep.","~school:  7.0<br />~log(`1960`):  6.810142<br />Egypt","~school:  1.1<br />~log(`1960`):  6.278521<br />Ethiopia","~school:  2.6<br />~log(`1960`):  7.175490<br />Gabon","~school:  1.5<br />~log(`1960`):  6.683361<br />Gambia, The","~school:  4.7<br />~log(`1960`):  6.916715<br />Ghana","~school:   NA<br />~log(`1960`):  6.614726<br />Guinea","~school:  2.3<br />~log(`1960`):  7.234177<br />Ivory Coast","~school:  2.4<br />~log(`1960`):  6.850126<br />Kenya","~school:  2.0<br />~log(`1960`):  6.066108<br />Lesotho","~school:  2.5<br />~log(`1960`):  6.760415<br />Liberia","~school:  2.6<br />~log(`1960`):  7.085064<br />Madagascar","~school:  0.6<br />~log(`1960`):  6.120297<br />Malawi","~school:  1.0<br />~log(`1960`):  6.602588<br />Mali","~school:  1.0<br />~log(`1960`):  6.655440<br />Mauritania","~school:  7.3<br />~log(`1960`):  7.587311<br />Mauritius","~school:  3.6<br />~log(`1960`):  6.937314<br />Morocco","~school:  0.7<br />~log(`1960`):  7.258412<br />Mozambique","~school:  0.5<br />~log(`1960`):  6.289716<br />Niger","~school:  2.3<br />~log(`1960`):  6.961296<br />Nigeria","~school:  0.4<br />~log(`1960`):  6.131226<br />Rwanda","~school:  1.7<br />~log(`1960`):  7.238497<br />Senegal","~school:  1.7<br />~log(`1960`):  6.236370<br />Sierra Leone","~school:  1.1<br />~log(`1960`):  6.803505<br />Somalia","~school:  3.0<br />~log(`1960`):  8.469682<br />S. Africa","~school:  2.0<br />~log(`1960`):  7.134094<br />Sudan","~school:  3.7<br />~log(`1960`):  6.705639<br />Swaziland","~school:  0.5<br />~log(`1960`):  5.948035<br />Tanzania","~school:  2.9<br />~log(`1960`):  6.655440<br />Togo","~school:  4.3<br />~log(`1960`):  7.392032<br />Tunisia","~school:  1.1<br />~log(`1960`):  6.398595<br />Uganda","~school:  3.6<br />~log(`1960`):  6.386879<br />Zaire","~school:  2.4<br />~log(`1960`):  7.251345<br />Zambia","~school:  4.4<br />~log(`1960`):  7.079184<br />Zimbabwe","~school:  0.9<br />~log(`1960`):  7.109879<br />Afghanistan","~school: 12.1<br />~log(`1960`):        NA<br />Bahrain","~school:  3.2<br />~log(`1960`):  6.740519<br />Bangladesh","~school:  3.5<br />~log(`1960`):  6.248043<br />Burma","~school:  7.2<br />~log(`1960`):  8.034307<br />Hong Kong","~school:  5.1<br />~log(`1960`):  6.885510<br />India","~school:  6.5<br />~log(`1960`):  8.190354<br />Iran","~school:  7.4<br />~log(`1960`):  8.500250<br />Iraq","~school:  9.5<br />~log(`1960`):  8.476788<br />Israel","~school: 10.9<br />~log(`1960`):  8.158516<br />Japan","~school: 10.8<br />~log(`1960`):  7.688455<br />Jordan","~school: 10.2<br />~log(`1960`):  7.158514<br />Korea, Rep. of","~school:  9.6<br />~log(`1960`): 11.262937<br />Kuwait","~school:  7.3<br />~log(`1960`):  7.675082<br />Malaysia","~school:  2.3<br />~log(`1960`):  6.725034<br />Nepal","~school:  2.7<br />~log(`1960`):        NA<br />Oman","~school:  3.0<br />~log(`1960`):  6.981935<br />Pakistan","~school: 10.6<br />~log(`1960`):  7.419381<br />Philippines","~school:  3.1<br />~log(`1960`):  8.814479<br />Saudi Arabia","~school:  9.0<br />~log(`1960`):  7.934872<br />Singapore","~school:  8.3<br />~log(`1960`):  7.492203<br />Sri Lanka","~school:  8.8<br />~log(`1960`):  7.775696<br />Syrian Arab Rep.","~school:   NA<br />~log(`1960`):        NA<br />Taiwan","~school:  4.4<br />~log(`1960`):  7.176255<br />Thailand","~school:   NA<br />~log(`1960`):        NA<br />U. Arab Emirates","~school:  0.6<br />~log(`1960`):        NA<br />Yemen","~school:  8.0<br />~log(`1960`):  8.689296<br />Austria","~school:  9.3<br />~log(`1960`):  8.823059<br />Belgium","~school:  8.2<br />~log(`1960`):  7.988882<br />Cyprus","~school: 10.7<br />~log(`1960`):  9.053804<br />Denmark","~school: 11.5<br />~log(`1960`):  8.783703<br />Finland","~school:  8.9<br />~log(`1960`):  8.883917<br />France","~school:  8.4<br />~log(`1960`):  8.948326<br />Germany, Fed. Rep.","~school:  7.9<br />~log(`1960`):  7.721792<br />Greece","~school: 10.2<br />~log(`1960`):  8.998508<br />Iceland","~school: 11.4<br />~log(`1960`):  8.391857<br />Ireland","~school:  7.1<br />~log(`1960`):  8.499640<br />Italy","~school:  5.0<br />~log(`1960`):  9.106645<br />Luxembourg","~school:  7.1<br />~log(`1960`):  7.737616<br />Malta","~school: 10.7<br />~log(`1960`):  8.947546<br />Netherlands","~school: 10.0<br />~log(`1960`):  8.979417<br />Norway"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(190,190,190,1)","opacity":0.2,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(190,190,190,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[5.8,8,7.9,4.8,5.5,8.9,12.1,10.6,7,5.8,3.9,2.4,1.9,3.7,11.2,6.6,5.8,11.6,8.8,11.9,5,4.9,4.7,7.7,6.1,7.2,11.7,4.4,8,8.1,7,7,9.8,8.1,4.1,11.9,1.5,4.5,1.8,1.8,2.9,0.4,0.4,3.4,1.4,0.4,3.8,7,1.1,2.6,1.5,4.7,null,2.3,2.4,2,2.5,2.6,0.6,1,1,7.3,3.6,0.7,0.5,2.3,0.4,1.7,1.7,1.1,3,2,3.7,0.5,2.9,4.3,1.1,3.6,2.4,4.4,0.9,12.1,3.2,3.5,7.2,5.1,6.5,7.4,9.5,10.9,10.8,10.2,9.6,7.3,2.3,2.7,3,10.6,3.1,9,8.3,8.8,null,4.4,null,0.6,8,9.3,8.2,10.7,11.5,8.9,8.4,7.9,10.2,11.4,7.1,5,7.1,10.7,10],"y":[7.72841577984104,8.2337687092171,8.9621353900666,9.24067557177226,7.72929567431048,8.94036723330517,8.05990833457828,9.23853902633486,8.11969625295725,7.56992765524265,7.62168499872461,7.8164169836918,6.99942246750796,7.26542972325395,7.91059061225648,8.34972083747249,8.06934236681164,7.79276172081653,9.13270310224903,9.42238253021922,8.48714627006394,7.38894609761844,7.51860721681525,8.55429627936774,7.89058253465654,7.69530313496357,7.92334821193015,7.57609734062311,8.10470346837111,8.07899825868515,8.54071438645758,9.24638296332311,9.04073758759,8.19808924895612,6.77878489768518,9.16146520419405,7.48493028328966,7.81802793853073,7.37023064180708,7.01750614294126,6.86589107488344,6.2709884318583,6.62671774924902,6.7900972355139,6.73101810048208,6.81124437860129,6.91671502035361,6.81014245011514,6.27852142416584,7.17548971362422,6.68336094576627,6.91671502035361,6.61472560020376,7.23417717974985,6.8501261661455,6.06610809010375,6.76041469108343,7.08506429395255,6.12029741895095,6.60258789218934,6.65544035036765,7.58731050602262,6.93731408122368,7.25841215059531,6.289715570909,6.96129604591017,6.13122648948314,7.23849684089437,6.2363695902037,6.80350525760834,8.46968220874519,7.13409372119287,6.70563909486,5.94803498918065,6.65544035036765,7.39203156751459,6.39859493453521,6.38687931936265,7.25134498337221,7.07918439460967,7.10987946307227,null,6.74051935960622,6.24804287450843,8.03430693633949,6.88550967003482,8.19035440376326,8.50025047068593,8.4767877767812,8.15851624480683,7.68845535654994,7.15851399732932,11.2629372996708,7.67508185771633,6.72503364216684,null,6.98193467715639,7.41938058291869,8.81447900001071,7.93487156594518,7.49220304261874,7.77569574991525,null,7.17625453201714,null,null,8.68929604801586,8.82305893430165,7.98888225330923,9.05380351415596,8.78370269863522,8.88391747120797,8.94832604627298,7.72179177681754,8.99850761180784,8.39185670010494,8.49964003216865,9.10664513563742,7.7376162828579,8.94754601503218,8.97941663334301],"text":["~school:  5.8<br />~log(`1960`):  7.728416<br />Portugal","~school:  8.0<br />~log(`1960`):  8.233769<br />Spain","~school:  7.9<br />~log(`1960`):  8.962135<br />Sweden","~school:  4.8<br />~log(`1960`):  9.240676<br />Switzerland","~school:  5.5<br />~log(`1960`):  7.729296<br />Turkey","~school:  8.9<br />~log(`1960`):  8.940367<br />United Kingdom","~school: 12.1<br />~log(`1960`):  8.059908<br />Barbados","~school: 10.6<br />~log(`1960`):  9.238539<br />Canada","~school:  7.0<br />~log(`1960`):  8.119696<br />Costa Rica","~school:  5.8<br />~log(`1960`):  7.569928<br />Dominican Rep.","~school:  3.9<br />~log(`1960`):  7.621685<br />El Salvador","~school:  2.4<br />~log(`1960`):  7.816417<br />Guatemala","~school:  1.9<br />~log(`1960`):  6.999422<br />Haiti","~school:  3.7<br />~log(`1960`):  7.265430<br />Honduras","~school: 11.2<br />~log(`1960`):  7.910591<br />Jamaica","~school:  6.6<br />~log(`1960`):  8.349721<br />Mexico","~school:  5.8<br />~log(`1960`):  8.069342<br />Nicaragua","~school: 11.6<br />~log(`1960`):  7.792762<br />Panama","~school:  8.8<br />~log(`1960`):  9.132703<br />Trinidad & Tobago","~school: 11.9<br />~log(`1960`):  9.422383<br />United States","~school:  5.0<br />~log(`1960`):  8.487146<br />Argentina","~school:  4.9<br />~log(`1960`):  7.388946<br />Bolivia","~school:  4.7<br />~log(`1960`):  7.518607<br />Brazil","~school:  7.7<br />~log(`1960`):  8.554296<br />Chile","~school:  6.1<br />~log(`1960`):  7.890583<br />Colombia","~school:  7.2<br />~log(`1960`):  7.695303<br />Ecuador","~school: 11.7<br />~log(`1960`):  7.923348<br />Guyana","~school:  4.4<br />~log(`1960`):  7.576097<br />Paraguay","~school:  8.0<br />~log(`1960`):  8.104703<br />Peru","~school:  8.1<br />~log(`1960`):  8.078998<br />Surinam","~school:  7.0<br />~log(`1960`):  8.540714<br />Uruguay","~school:  7.0<br />~log(`1960`):  9.246383<br />Venezuela","~school:  9.8<br />~log(`1960`):  9.040738<br />Australia","~school:  8.1<br />~log(`1960`):  8.198089<br />Fiji","~school:  4.1<br />~log(`1960`):  6.778785<br />Indonesia","~school: 11.9<br />~log(`1960`):  9.161465<br />New Zealand","~school:  1.5<br />~log(`1960`):  7.484930<br />Papua New Guinea","~school:  4.5<br />~log(`1960`):  7.818028<br />Algeria","~school:  1.8<br />~log(`1960`):  7.370231<br />Angola","~school:  1.8<br />~log(`1960`):  7.017506<br />Benin","~school:  2.9<br />~log(`1960`):  6.865891<br />Botswana","~school:  0.4<br />~log(`1960`):  6.270988<br />Burkina Faso","~school:  0.4<br />~log(`1960`):  6.626718<br />Burundi","~school:  3.4<br />~log(`1960`):  6.790097<br />Cameroon","~school:  1.4<br />~log(`1960`):  6.731018<br />CentralAfr. Rep.","~school:  0.4<br />~log(`1960`):  6.811244<br />Chad","~school:  3.8<br />~log(`1960`):  6.916715<br />Congo, Peop. Rep.","~school:  7.0<br />~log(`1960`):  6.810142<br />Egypt","~school:  1.1<br />~log(`1960`):  6.278521<br />Ethiopia","~school:  2.6<br />~log(`1960`):  7.175490<br />Gabon","~school:  1.5<br />~log(`1960`):  6.683361<br />Gambia, The","~school:  4.7<br />~log(`1960`):  6.916715<br />Ghana","~school:   NA<br />~log(`1960`):  6.614726<br />Guinea","~school:  2.3<br />~log(`1960`):  7.234177<br />Ivory Coast","~school:  2.4<br />~log(`1960`):  6.850126<br />Kenya","~school:  2.0<br />~log(`1960`):  6.066108<br />Lesotho","~school:  2.5<br />~log(`1960`):  6.760415<br />Liberia","~school:  2.6<br />~log(`1960`):  7.085064<br />Madagascar","~school:  0.6<br />~log(`1960`):  6.120297<br />Malawi","~school:  1.0<br />~log(`1960`):  6.602588<br />Mali","~school:  1.0<br />~log(`1960`):  6.655440<br />Mauritania","~school:  7.3<br />~log(`1960`):  7.587311<br />Mauritius","~school:  3.6<br />~log(`1960`):  6.937314<br />Morocco","~school:  0.7<br />~log(`1960`):  7.258412<br />Mozambique","~school:  0.5<br />~log(`1960`):  6.289716<br />Niger","~school:  2.3<br />~log(`1960`):  6.961296<br />Nigeria","~school:  0.4<br />~log(`1960`):  6.131226<br />Rwanda","~school:  1.7<br />~log(`1960`):  7.238497<br />Senegal","~school:  1.7<br />~log(`1960`):  6.236370<br />Sierra Leone","~school:  1.1<br />~log(`1960`):  6.803505<br />Somalia","~school:  3.0<br />~log(`1960`):  8.469682<br />S. Africa","~school:  2.0<br />~log(`1960`):  7.134094<br />Sudan","~school:  3.7<br />~log(`1960`):  6.705639<br />Swaziland","~school:  0.5<br />~log(`1960`):  5.948035<br />Tanzania","~school:  2.9<br />~log(`1960`):  6.655440<br />Togo","~school:  4.3<br />~log(`1960`):  7.392032<br />Tunisia","~school:  1.1<br />~log(`1960`):  6.398595<br />Uganda","~school:  3.6<br />~log(`1960`):  6.386879<br />Zaire","~school:  2.4<br />~log(`1960`):  7.251345<br />Zambia","~school:  4.4<br />~log(`1960`):  7.079184<br />Zimbabwe","~school:  0.9<br />~log(`1960`):  7.109879<br />Afghanistan","~school: 12.1<br />~log(`1960`):        NA<br />Bahrain","~school:  3.2<br />~log(`1960`):  6.740519<br />Bangladesh","~school:  3.5<br />~log(`1960`):  6.248043<br />Burma","~school:  7.2<br />~log(`1960`):  8.034307<br />Hong Kong","~school:  5.1<br />~log(`1960`):  6.885510<br />India","~school:  6.5<br />~log(`1960`):  8.190354<br />Iran","~school:  7.4<br />~log(`1960`):  8.500250<br />Iraq","~school:  9.5<br />~log(`1960`):  8.476788<br />Israel","~school: 10.9<br />~log(`1960`):  8.158516<br />Japan","~school: 10.8<br />~log(`1960`):  7.688455<br />Jordan","~school: 10.2<br />~log(`1960`):  7.158514<br />Korea, Rep. of","~school:  9.6<br />~log(`1960`): 11.262937<br />Kuwait","~school:  7.3<br />~log(`1960`):  7.675082<br />Malaysia","~school:  2.3<br />~log(`1960`):  6.725034<br />Nepal","~school:  2.7<br />~log(`1960`):        NA<br />Oman","~school:  3.0<br />~log(`1960`):  6.981935<br />Pakistan","~school: 10.6<br />~log(`1960`):  7.419381<br />Philippines","~school:  3.1<br />~log(`1960`):  8.814479<br />Saudi Arabia","~school:  9.0<br />~log(`1960`):  7.934872<br />Singapore","~school:  8.3<br />~log(`1960`):  7.492203<br />Sri Lanka","~school:  8.8<br />~log(`1960`):  7.775696<br />Syrian Arab Rep.","~school:   NA<br />~log(`1960`):        NA<br />Taiwan","~school:  4.4<br />~log(`1960`):  7.176255<br />Thailand","~school:   NA<br />~log(`1960`):        NA<br />U. Arab Emirates","~school:  0.6<br />~log(`1960`):        NA<br />Yemen","~school:  8.0<br />~log(`1960`):  8.689296<br />Austria","~school:  9.3<br />~log(`1960`):  8.823059<br />Belgium","~school:  8.2<br />~log(`1960`):  7.988882<br />Cyprus","~school: 10.7<br />~log(`1960`):  9.053804<br />Denmark","~school: 11.5<br />~log(`1960`):  8.783703<br />Finland","~school:  8.9<br />~log(`1960`):  8.883917<br />France","~school:  8.4<br />~log(`1960`):  8.948326<br />Germany, Fed. Rep.","~school:  7.9<br />~log(`1960`):  7.721792<br />Greece","~school: 10.2<br />~log(`1960`):  8.998508<br />Iceland","~school: 11.4<br />~log(`1960`):  8.391857<br />Ireland","~school:  7.1<br />~log(`1960`):  8.499640<br />Italy","~school:  5.0<br />~log(`1960`):  9.106645<br />Luxembourg","~school:  7.1<br />~log(`1960`):  7.737616<br />Malta","~school: 10.7<br />~log(`1960`):  8.947546<br />Netherlands","~school: 10.0<br />~log(`1960`):  8.979417<br />Norway"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(190,190,190,1)","opacity":0.2,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(190,190,190,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[12.1,7,5.8,3.9,2.4,1.9,3.7,11.2,6.6,5.8,11.6,8.8,5,4.9,4.7,7.7,6.1,7.2,11.7,4.4,8,8.1,7,7,8.1,4.1,1.5,4.5,1.8,1.8,2.9,0.4,0.4,3.4,1.4,0.4,3.8,7,1.1,2.6,1.5,4.7,null,2.3,2.4,2,2.5,2.6,0.6,1,1,7.3,3.6,0.7,0.5,2.3,0.4,1.7,1.7,1.1,3,2,3.7,0.5,2.9,4.3,1.1,3.6,2.4,4.4,0.9,12.1,3.2,3.5,7.2,5.1,6.5,7.4,9.5,10.8,10.2,9.6,7.3,2.3,2.7,3,10.6,3.1,9,8.3,8.8,null,4.4,null,0.6,8.2,10.2,5,7.1],"y":[8.05990833457828,8.11969625295725,7.56992765524265,7.62168499872461,7.8164169836918,6.99942246750796,7.26542972325395,7.91059061225648,8.34972083747249,8.06934236681164,7.79276172081653,9.13270310224903,8.48714627006394,7.38894609761844,7.51860721681525,8.55429627936774,7.89058253465654,7.69530313496357,7.92334821193015,7.57609734062311,8.10470346837111,8.07899825868515,8.54071438645758,9.24638296332311,8.19808924895612,6.77878489768518,7.48493028328966,7.81802793853073,7.37023064180708,7.01750614294126,6.86589107488344,6.2709884318583,6.62671774924902,6.7900972355139,6.73101810048208,6.81124437860129,6.91671502035361,6.81014245011514,6.27852142416584,7.17548971362422,6.68336094576627,6.91671502035361,6.61472560020376,7.23417717974985,6.8501261661455,6.06610809010375,6.76041469108343,7.08506429395255,6.12029741895095,6.60258789218934,6.65544035036765,7.58731050602262,6.93731408122368,7.25841215059531,6.289715570909,6.96129604591017,6.13122648948314,7.23849684089437,6.2363695902037,6.80350525760834,8.46968220874519,7.13409372119287,6.70563909486,5.94803498918065,6.65544035036765,7.39203156751459,6.39859493453521,6.38687931936265,7.25134498337221,7.07918439460967,7.10987946307227,null,6.74051935960622,6.24804287450843,8.03430693633949,6.88550967003482,8.19035440376326,8.50025047068593,8.4767877767812,7.68845535654994,7.15851399732932,11.2629372996708,7.67508185771633,6.72503364216684,null,6.98193467715639,7.41938058291869,8.81447900001071,7.93487156594518,7.49220304261874,7.77569574991525,null,7.17625453201714,null,null,7.98888225330923,8.99850761180784,9.10664513563742,7.7376162828579],"text":["~school: 12.1<br />~log(`1960`):  8.059908<br />Barbados","~school:  7.0<br />~log(`1960`):  8.119696<br />Costa Rica","~school:  5.8<br />~log(`1960`):  7.569928<br />Dominican Rep.","~school:  3.9<br />~log(`1960`):  7.621685<br />El Salvador","~school:  2.4<br />~log(`1960`):  7.816417<br />Guatemala","~school:  1.9<br />~log(`1960`):  6.999422<br />Haiti","~school:  3.7<br />~log(`1960`):  7.265430<br />Honduras","~school: 11.2<br />~log(`1960`):  7.910591<br />Jamaica","~school:  6.6<br />~log(`1960`):  8.349721<br />Mexico","~school:  5.8<br />~log(`1960`):  8.069342<br />Nicaragua","~school: 11.6<br />~log(`1960`):  7.792762<br />Panama","~school:  8.8<br />~log(`1960`):  9.132703<br />Trinidad & Tobago","~school:  5.0<br />~log(`1960`):  8.487146<br />Argentina","~school:  4.9<br />~log(`1960`):  7.388946<br />Bolivia","~school:  4.7<br />~log(`1960`):  7.518607<br />Brazil","~school:  7.7<br />~log(`1960`):  8.554296<br />Chile","~school:  6.1<br />~log(`1960`):  7.890583<br />Colombia","~school:  7.2<br />~log(`1960`):  7.695303<br />Ecuador","~school: 11.7<br />~log(`1960`):  7.923348<br />Guyana","~school:  4.4<br />~log(`1960`):  7.576097<br />Paraguay","~school:  8.0<br />~log(`1960`):  8.104703<br />Peru","~school:  8.1<br />~log(`1960`):  8.078998<br />Surinam","~school:  7.0<br />~log(`1960`):  8.540714<br />Uruguay","~school:  7.0<br />~log(`1960`):  9.246383<br />Venezuela","~school:  8.1<br />~log(`1960`):  8.198089<br />Fiji","~school:  4.1<br />~log(`1960`):  6.778785<br />Indonesia","~school:  1.5<br />~log(`1960`):  7.484930<br />Papua New Guinea","~school:  4.5<br />~log(`1960`):  7.818028<br />Algeria","~school:  1.8<br />~log(`1960`):  7.370231<br />Angola","~school:  1.8<br />~log(`1960`):  7.017506<br />Benin","~school:  2.9<br />~log(`1960`):  6.865891<br />Botswana","~school:  0.4<br />~log(`1960`):  6.270988<br />Burkina Faso","~school:  0.4<br />~log(`1960`):  6.626718<br />Burundi","~school:  3.4<br />~log(`1960`):  6.790097<br />Cameroon","~school:  1.4<br />~log(`1960`):  6.731018<br />CentralAfr. Rep.","~school:  0.4<br />~log(`1960`):  6.811244<br />Chad","~school:  3.8<br />~log(`1960`):  6.916715<br />Congo, Peop. Rep.","~school:  7.0<br />~log(`1960`):  6.810142<br />Egypt","~school:  1.1<br />~log(`1960`):  6.278521<br />Ethiopia","~school:  2.6<br />~log(`1960`):  7.175490<br />Gabon","~school:  1.5<br />~log(`1960`):  6.683361<br />Gambia, The","~school:  4.7<br />~log(`1960`):  6.916715<br />Ghana","~school:   NA<br />~log(`1960`):  6.614726<br />Guinea","~school:  2.3<br />~log(`1960`):  7.234177<br />Ivory Coast","~school:  2.4<br />~log(`1960`):  6.850126<br />Kenya","~school:  2.0<br />~log(`1960`):  6.066108<br />Lesotho","~school:  2.5<br />~log(`1960`):  6.760415<br />Liberia","~school:  2.6<br />~log(`1960`):  7.085064<br />Madagascar","~school:  0.6<br />~log(`1960`):  6.120297<br />Malawi","~school:  1.0<br />~log(`1960`):  6.602588<br />Mali","~school:  1.0<br />~log(`1960`):  6.655440<br />Mauritania","~school:  7.3<br />~log(`1960`):  7.587311<br />Mauritius","~school:  3.6<br />~log(`1960`):  6.937314<br />Morocco","~school:  0.7<br />~log(`1960`):  7.258412<br />Mozambique","~school:  0.5<br />~log(`1960`):  6.289716<br />Niger","~school:  2.3<br />~log(`1960`):  6.961296<br />Nigeria","~school:  0.4<br />~log(`1960`):  6.131226<br />Rwanda","~school:  1.7<br />~log(`1960`):  7.238497<br />Senegal","~school:  1.7<br />~log(`1960`):  6.236370<br />Sierra Leone","~school:  1.1<br />~log(`1960`):  6.803505<br />Somalia","~school:  3.0<br />~log(`1960`):  8.469682<br />S. Africa","~school:  2.0<br />~log(`1960`):  7.134094<br />Sudan","~school:  3.7<br />~log(`1960`):  6.705639<br />Swaziland","~school:  0.5<br />~log(`1960`):  5.948035<br />Tanzania","~school:  2.9<br />~log(`1960`):  6.655440<br />Togo","~school:  4.3<br />~log(`1960`):  7.392032<br />Tunisia","~school:  1.1<br />~log(`1960`):  6.398595<br />Uganda","~school:  3.6<br />~log(`1960`):  6.386879<br />Zaire","~school:  2.4<br />~log(`1960`):  7.251345<br />Zambia","~school:  4.4<br />~log(`1960`):  7.079184<br />Zimbabwe","~school:  0.9<br />~log(`1960`):  7.109879<br />Afghanistan","~school: 12.1<br />~log(`1960`):        NA<br />Bahrain","~school:  3.2<br />~log(`1960`):  6.740519<br />Bangladesh","~school:  3.5<br />~log(`1960`):  6.248043<br />Burma","~school:  7.2<br />~log(`1960`):  8.034307<br />Hong Kong","~school:  5.1<br />~log(`1960`):  6.885510<br />India","~school:  6.5<br />~log(`1960`):  8.190354<br />Iran","~school:  7.4<br />~log(`1960`):  8.500250<br />Iraq","~school:  9.5<br />~log(`1960`):  8.476788<br />Israel","~school: 10.8<br />~log(`1960`):  7.688455<br />Jordan","~school: 10.2<br />~log(`1960`):  7.158514<br />Korea, Rep. of","~school:  9.6<br />~log(`1960`): 11.262937<br />Kuwait","~school:  7.3<br />~log(`1960`):  7.675082<br />Malaysia","~school:  2.3<br />~log(`1960`):  6.725034<br />Nepal","~school:  2.7<br />~log(`1960`):        NA<br />Oman","~school:  3.0<br />~log(`1960`):  6.981935<br />Pakistan","~school: 10.6<br />~log(`1960`):  7.419381<br />Philippines","~school:  3.1<br />~log(`1960`):  8.814479<br />Saudi Arabia","~school:  9.0<br />~log(`1960`):  7.934872<br />Singapore","~school:  8.3<br />~log(`1960`):  7.492203<br />Sri Lanka","~school:  8.8<br />~log(`1960`):  7.775696<br />Syrian Arab Rep.","~school:   NA<br />~log(`1960`):        NA<br />Taiwan","~school:  4.4<br />~log(`1960`):  7.176255<br />Thailand","~school:   NA<br />~log(`1960`):        NA<br />U. Arab Emirates","~school:  0.6<br />~log(`1960`):        NA<br />Yemen","~school:  8.2<br />~log(`1960`):  7.988882<br />Cyprus","~school: 10.2<br />~log(`1960`):  8.998508<br />Iceland","~school:  5.0<br />~log(`1960`):  9.106645<br />Luxembourg","~school:  7.1<br />~log(`1960`):  7.737616<br />Malta"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,20,147,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,20,147,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[5.8,8,7.9,4.8,5.5,8.9,10.6,11.9,9.8,11.9,10.9,8,9.3,10.7,11.5,8.9,8.4,7.9,11.4,7.1,10.7,10],"y":[7.72841577984104,8.2337687092171,8.9621353900666,9.24067557177226,7.72929567431048,8.94036723330517,9.23853902633486,9.42238253021922,9.04073758759,9.16146520419405,8.15851624480683,8.68929604801586,8.82305893430165,9.05380351415596,8.78370269863522,8.88391747120797,8.94832604627298,7.72179177681754,8.39185670010494,8.49964003216865,8.94754601503218,8.97941663334301],"text":["~school:  5.8<br />~log(`1960`):  7.728416<br />Portugal","~school:  8.0<br />~log(`1960`):  8.233769<br />Spain","~school:  7.9<br />~log(`1960`):  8.962135<br />Sweden","~school:  4.8<br />~log(`1960`):  9.240676<br />Switzerland","~school:  5.5<br />~log(`1960`):  7.729296<br />Turkey","~school:  8.9<br />~log(`1960`):  8.940367<br />United Kingdom","~school: 10.6<br />~log(`1960`):  9.238539<br />Canada","~school: 11.9<br />~log(`1960`):  9.422383<br />United States","~school:  9.8<br />~log(`1960`):  9.040738<br />Australia","~school: 11.9<br />~log(`1960`):  9.161465<br />New Zealand","~school: 10.9<br />~log(`1960`):  8.158516<br />Japan","~school:  8.0<br />~log(`1960`):  8.689296<br />Austria","~school:  9.3<br />~log(`1960`):  8.823059<br />Belgium","~school: 10.7<br />~log(`1960`):  9.053804<br />Denmark","~school: 11.5<br />~log(`1960`):  8.783703<br />Finland","~school:  8.9<br />~log(`1960`):  8.883917<br />France","~school:  8.4<br />~log(`1960`):  8.948326<br />Germany, Fed. Rep.","~school:  7.9<br />~log(`1960`):  7.721792<br />Greece","~school: 11.4<br />~log(`1960`):  8.391857<br />Ireland","~school:  7.1<br />~log(`1960`):  8.499640<br />Italy","~school: 10.7<br />~log(`1960`):  8.947546<br />Netherlands","~school: 10.0<br />~log(`1960`):  8.979417<br />Norway"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,20,147,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,20,147,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":55.4520547945205,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Schooling vs GDP in OECD and non-OECD countries","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,0.489128071319852],"type":"linear","autorange":false,"range":[-0.185,12.685],"tickmode":"array","ticktext":["0.0","2.5","5.0","7.5","10.0","12.5"],"tickvals":[0,2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["0.0","2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"annotations":[{"text":"school","x":0.5,"y":-0.0471841704718417,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"Log Real GDP pc in 1960","x":-0.0337029789084584,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"0","x":0.244564035659926,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"1","x":0.755435964340074,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"range":[5.68228987365614,11.5286824151953],"tickmode":"array","ticktext":["6","8","10"],"tickvals":[6,8,10],"categoryorder":"array","categoryarray":["6","8","10"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":1,"y1":1.06929133858268},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":1,"y1":1.06929133858268}],"xaxis2":{"type":"linear","autorange":false,"range":[-0.185,12.685],"tickmode":"array","ticktext":["0.0","2.5","5.0","7.5","10.0","12.5"],"tickvals":[0,2.5,5,7.5,10,12.5],"categoryorder":"array","categoryarray":["0.0","2.5","5.0","7.5","10.0","12.5"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.510871928680148,1],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"222d01e1a21e2":{"x":{},"y":{},"text":{},"type":"scatter"},"222d012b6239":{"x":{},"y":{},"text":{}}},"cur_data":"222d01e1a21e2","visdat":{"222d01e1a21e2":["function (y) ","x"],"222d012b6239":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->

```r
p <- plot_ly(data = MRW_clean,
             x = ~s,
             y = ~`1985`,
             size = ~school,
             text = ~country,
             type = 'scatter',
             mode = 'markers'
             ) %>% 
  layout(yaxis = list(title = 'Log Real GDP pc in 1985'),
         xaxis = list(title = 'Savings rate (I/Y)'),
         title = 'GDP vs Savings rate \n size = schooling')
p
```

<!--html_preserve--><div id="222d0478725a2" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="222d0478725a2">{"x":{"visdat":{"222d03c604b2f":["function () ","plotlyVisDat"]},"cur_data":"222d03c604b2f","attrs":{"222d03c604b2f":{"x":{},"y":{},"text":{},"mode":"markers","size":{},"alpha":1,"sizes":[10,100],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"yaxis":{"domain":[0,1],"title":"Log Real GDP pc in 1985"},"xaxis":{"domain":[0,1],"title":"Savings rate (I/Y)"},"title":"GDP vs Savings rate <br /> size = schooling","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"data":[{"x":[22.5,17.7,24.5,29.7,20.2,18.4,23.3,14.7,17.1,8,8.8,7.1,13.8,20.6,19.5,14.5,26.1,20.4,21.1,25.3,13.3,23.2,29.7,18,24.4,11.7,12,11.8,11.4,31.5,13.9,22.5,16.2,24.1,5.8,10.8,28.3,12.7,5.1,12.8,10.5,6.9,28.8,16.3,5.4,22.1,9.1,10.9,12.4,17.4,12.6,21.5,7.1,13.2,7.3,25.6,17.1,8.3,6.1,10.3,12,7.9,9.6,10.9,13.8,21.6,13.2,18,15.5,13.8,4.1,6.5,31.7,21.1,6.8,11.4,19.9,16.8,18.4,16.2,28.5,36,17.6,22.3,9.5,23.2,5.9,15.6,12.2,14.9,12.8,32.2,14.8,15.9,18,26.5,17.2,23.4,23.4,26.6,36.9,26.2,28.5,29.3,25.9,24.9,25.8,29.1],"y":[5827,9903,15237,15881,4444,13331,17935,4492,3308,1997,3034,1237,1822,3080,7380,3978,5021,11285,18988,5533,2055,5563,5533,4405,4504,3914,3775,5495,6336,13409,2159,12308,2544,4371,1171,1071,3671,857,663,2190,789,462,2624,2160,608,5350,727,869,1704,1329,1483,944,975,823,710,1038,2967,2348,1035,841,1186,696,1450,805,657,7064,1038,710,978,3661,667,412,1217,2107,1221,1031,13372,1339,7400,5626,10450,13893,4312,4775,25635,5788,974,15584,2175,2430,11057,14678,2482,6042,3220,18513,1918,13327,14290,16491,13779,15027,15297,6868,8675,11082,13177,19723],"text":["Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","Canada","Costa Rica","Dominican Rep.","El Salvador","Guatemala","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Trinidad & Tobago","United States","Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Paraguay","Peru","Uruguay","Venezuela","Australia","Indonesia","New Zealand","Papua New Guinea","Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","CentralAfr. Rep.","Chad","Congo, Peop. Rep.","Egypt","Ethiopia","Gabon","Ghana","Guinea","Ivory Coast","Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Niger","Nigeria","Rwanda","Senegal","Sierra Leone","Somalia","S. Africa","Sudan","Tanzania","Togo","Tunisia","Uganda","Zaire","Zambia","Zimbabwe","Bangladesh","Burma","Hong Kong","India","Iran","Iraq","Israel","Japan","Jordan","Korea, Rep. of","Kuwait","Malaysia","Nepal","Oman","Pakistan","Philippines","Saudi Arabia","Singapore","Sri Lanka","Syrian Arab Rep.","Thailand","U. Arab Emirates","Yemen","Austria","Belgium","Denmark","Finland","France","Germany, Fed. Rep.","Greece","Ireland","Italy","Netherlands","Norway"],"mode":"markers","type":"scatter","marker":{"size":[52.2608695652174,69.4782608695652,68.695652173913,44.4347826086956,49.9130434782609,76.5217391304348,89.8260869565217,61.6521739130435,52.2608695652174,37.3913043478261,25.6521739130435,21.7391304347826,35.8260869565217,94.5217391304348,58.5217391304348,52.2608695652174,97.6521739130435,75.7391304347826,100,46,45.2173913043478,43.6521739130435,67.1304347826087,54.6086956521739,63.2173913043478,41.304347826087,69.4782608695652,61.6521739130435,61.6521739130435,83.5652173913044,38.9565217391304,100,18.6086956521739,42.0869565217391,20.9565217391304,20.9565217391304,29.5652173913043,10,10,33.4782608695652,17.8260869565217,10,36.6086956521739,61.6521739130435,15.4782608695652,27.2173913043478,43.6521739130435,null,24.8695652173913,25.6521739130435,22.5217391304348,26.4347826086957,27.2173913043478,11.5652173913043,14.695652173913,14.695652173913,64,35.0434782608696,12.3478260869565,10.7826086956522,24.8695652173913,10,20.1739130434783,20.1739130434783,15.4782608695652,30.3478260869565,22.5217391304348,10.7826086956522,29.5652173913043,40.5217391304348,15.4782608695652,35.0434782608696,25.6521739130435,41.304347826087,31.9130434782609,34.2608695652174,63.2173913043478,46.7826086956522,57.7391304347826,64.7826086956522,81.2173913043478,92.1739130434783,91.3913043478261,86.695652173913,82,64,24.8695652173913,28,30.3478260869565,89.8260869565217,31.1304347826087,77.304347826087,71.8260869565217,75.7391304347826,41.304347826087,null,11.5652173913043,69.4782608695652,79.6521739130435,90.6086956521739,96.8695652173913,76.5217391304348,72.6086956521739,68.695652173913,96.0869565217391,62.4347826086956,90.6086956521739,85.1304347826087],"sizemode":"area","fillcolor":"rgba(31,119,180,1)","color":"rgba(31,119,180,1)","line":{"color":"transparent"}},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->

## Estimating the Solow Model and Augmented Solow Model

The regression equation Mankiw, Romer and Weil estimate can be written as $ln(GDP_i) = \alpha + ln(s_i) + ln(n_i+g_i+d_i) + e_i$ where GDP is actually GDP per working age person (although the appendix displays this as GDP/adult in the interests of brevity); $s_i$, the savings rate,  is calculated using investment as a share of GDP; $n_i$ is the average change in working age population between 1960 and 1985 and $g_i+d_i$ are assumed to sum to 0.05.

The augmented model is the same but we add in a `school` variable to control for human capital.



Formulating the above in R and splitting countries into subsets gives:


```r
solow_formula <- log(`1985`) ~ log(s) + log(`n_g_d`)

## Non-Oil
non_oil_data <- MRW_clean %>% 
  filter(N == 1) %>% 
  filter(!is.na(school)) # MRW drop missing school observations to make comparisons easier

non_oil_model <- lm(solow_formula,
                    data = non_oil_data)

## Intermediate
intermediate_data <- MRW_clean %>% 
  filter(I == 1) %>% 
  filter(!is.na(school))

intermediate_model <- lm(solow_formula,
                         data = intermediate_data)

## OECD
oecd_data <- MRW_clean %>% 
  filter(O == 1) %>% 
  filter(!is.na(school))

oecd_model <- lm(solow_formula,
                 data = oecd_data)

#### Augmented Model
augmented_formula <- log(`1985`) ~ log(s) + log(n_g_d) + log(school)

## Non-Oil
aug_non_oil_model <- lm(augmented_formula,
                        data = non_oil_data)


## Intermediate
aug_intermediate_model <- lm(augmented_formula,
                             data = intermediate_data)


## OECD

aug_oecd_model <- lm(augmented_formula,
                     data = oecd_data)
```

Now collecting and comparing results using broom's `tidy`. We use `map2_dfr` because we want each observation to have a variable describing which model it came from and I haven't figured out a simpler way to do this:

```r
library(broom)
collect.results <- function(models, names){
  
  results <- map2_dfr(.x = models,
                      .y = names,
                      .f = function(x, y){tidy(x) %>% mutate(subset = y)}) %>% 
    as_tibble()
  return(results)
}

solow_rep <- collect.results(list(non_oil_model,
                                 intermediate_model,
                                 oecd_model),
                            list('Non-Oil',
                                 'Intermediate',
                                 'OECD'))

augmented_rep <- collect.results(list(aug_non_oil_model,
                                     aug_intermediate_model,
                                     aug_oecd_model),
                                list('Non-Oil',
                                     'Intermediate',
                                     'OECD'))

solow_MRW <- tribble(
  ~term, ~estimate, ~std.error, ~statistic, ~p.value, ~subset,
  '(Intercept)', 5.48, 1.59, NA, NA, 'Non-Oil',
  'log(s)', 1.42, 0.14, NA, NA, 'Non-Oil',
  'log(n_g_d)', -1.97, 0.56, NA, NA, 'Non-Oil',
  '(Intercept)', 5.36, 1.55, NA, NA, 'Intermediate',
  'log(s)', 1.31, 0.17, NA, NA, 'Intermediate',
  'log(n_g_d)', -2.01, 0.53, NA, NA, 'Intermediate',
  '(Intercept)', 7.97, 2.48, NA, NA, 'OECD',
  'log(s)', 0.5, 0.43, NA, NA, 'OECD',
  'log(n_g_d)', -0.76, 0.84, NA, NA, 'OECD'
)

augmented_MRW <- tribble(
  ~term, ~estimate, ~std.error, ~statistic, ~p.value, ~subset,
  '(Intercept)', 6.89, 1.17, NA, NA, 'Non-Oil',
   'log(s)',  0.69, 0.13, NA, NA, 'Non-Oil',
  'log(n_g_d)', -1.73, 0.41, NA, NA, 'Non-Oil',
  'log(school)', 0.66, 0.07, NA, NA, 'Non-Oil',
  '(Intercept)', 7.81, 1.19, NA, NA, 'Intermediate',
  'log(s)', 0.7, 0.15, NA, NA, 'Intermediate',
  'log(n_g_d)', -1.5, 0.4, NA, NA, 'Intermediate',
  'log(school)', 0.73, 0.1, NA, NA, 'Intermediate',
  '(Intercept)', 8.63, 2.19, NA, NA, 'OECD',
  'log(s)', 0.28, 0.39, NA, NA, 'OECD',
  'log(n_g_d)', -1.07, 0.75, NA, NA, 'OECD',
  'log(school)', 0.76, 0.29, NA, NA, 'OECD'
)
```
The MRW data comes from just manually transferring estimates and standard errors from Table 1 and Table 2 into a tibble using `tribble`.


## Comparing Results

We need to collect parameters from each of our models and compare between replicated and original estimates. This function selects the right parameter (and re-orders them for easier comparison); merges the two dataframe estimates into one and calculates the estimated differences. We use `grep` to find replicated and original columns and `pull` lets us access the values in each selected tibble (I'm not entirely sure why we can't subtract tibble A from tibble B here):

```r
compare.results <- function(parameter, parameter_type = 'estimate', results_replicated, results_original){
  replicated_comparison <- results_replicated %>% 
    filter(term == parameter) %>% 
    select(term, subset, parameter_type)
  
  original_comparison <- results_original %>% 
    filter(term == parameter) %>% 
    select(term, subset, parameter_type)
  
  comparison_df <- full_join(x = replicated_comparison,
                             y = original_comparison,
                             by = c('term', 'subset'),
                             suffix = c('_replicated', '_original'))
  comparison_df$diff <- comparison_df[, grep('_original', colnames(comparison_df))] %>% 
    pull - comparison_df[, grep('_replicated', colnames(comparison_df))] %>% 
    pull
  comparison_df$rounded_diff <- round(comparison_df$diff, 2)
  comparison_df$pct_orig <- comparison_df$diff / comparison_df[, grep('_original', colnames(comparison_df))] %>% pull * 100
  return(comparison_df)
}
```
 We then use `map_df` to apply the function to each parameter type and collect our output in a dataframe:

```r
parameters <- c('(Intercept)',
                'log(s)',
                'log(n_g_d)')

solow_comparison_estimates <- parameters %>% 
  map_df(compare.results,
         parameter_type = 'estimate',
         results_replicated = solow_rep,
         results_original = solow_MRW)

augmented_comparison_estimates <- parameters %>% 
  map_df(compare.results,
         parameter_type = 'estimate',
         results_replicated = augmented_rep,
         results_original = augmented_MRW)


solow_comparison_std_error <- parameters %>% 
  map_df(compare.results,
         parameter_type = 'std.error',
         results_replicated = solow_rep,
         results_original = solow_MRW)

augmented_comparison_std_error<- parameters %>% 
  map_df(compare.results,
         parameter_type = 'std.error',
         results_replicated = augmented_rep,
         results_original = augmented_MRW)
```

Looking at Table 1's replicated estimates is a mixed bag:


term          subset          estimate_replicated   estimate_original         diff   rounded_diff      pct_orig
------------  -------------  --------------------  ------------------  -----------  -------------  ------------
(Intercept)   Non-Oil                   8.0353051                5.48   -2.5553051          -2.56   -46.6296552
(Intercept)   Intermediate              8.5678580                5.36   -3.2078580          -3.21   -59.8480966
(Intercept)   OECD                      9.1352049                7.97   -1.1652049          -1.17   -14.6198864
log(s)        Non-Oil                   1.4240143                1.42   -0.0040143           0.00    -0.2826960
log(s)        Intermediate              1.3175527                1.31   -0.0075527          -0.01    -0.5765386
log(s)        OECD                      0.4998895                0.50    0.0001105           0.00     0.0220925
log(n_g_d)    Non-Oil                  -1.9897745               -1.97    0.0197745           0.02    -1.0037809
log(n_g_d)    Intermediate             -2.0171995               -2.01    0.0071995           0.01    -0.3581838
log(n_g_d)    OECD                     -0.7419215               -0.76   -0.0180785          -0.02     2.3787554

The variable estimates are on point but we're pretty off on the intercept. In fact, the same is true for both the standard errors and the augmented Solow model (which can also be found on GitHub.):


```r
all_estimates <- bind_rows(solow_comparison_estimates,
                         augmented_comparison_estimates)
all_std_errors <- bind_rows(solow_comparison_std_error,
                            augmented_comparison_std_error)

mean_est_diff <- all_estimates %>% 
  group_by(term) %>% 
  summarise(mean(diff),
            mean(pct_orig))
mean_est_diff
```

```
# A tibble: 3 x 3
  term        `mean(diff)` `mean(pct_orig)`
  <chr>              <dbl>            <dbl>
1 (Intercept)     -1.52            -25.4   
2 log(n_g_d)       0.00490          -0.0608
3 log(s)          -0.00244          -0.0802
```

```r
mean_std_diff <- all_std_errors %>% 
  group_by(term) %>% 
  summarise(mean(diff),
            mean(pct_orig))
mean_std_diff
```

```
# A tibble: 3 x 3
  term        `mean(diff)` `mean(pct_orig)`
  <chr>              <dbl>            <dbl>
1 (Intercept)      0.272             16.7  
2 log(n_g_d)      -0.00576           -0.972
3 log(s)          -0.00171           -0.995
```


## Conclusion


Our replicated intercepts are off on average by -1.525 in absolute terms or -25.4\%.^[This metric is a little tricky to interpret since we're using different models and three different subsets of countries.] 

Everything else, however, is pretty spot on. So, what's happening here?

The fact that the variable estimates are near perfect i.e. we're identifying the same slope as MRW but the wrong 'location' points to a couple of things. Since the intercept is off and there was a little confusion as to how add a constant to the model, recall MRW's +0.05 for depreciation and technology growth, it seems plausible at first glance that we've got the wrong scale when we added +5 to `working_age_pop_ch` and should have perhaps added 0.05. 

However, I don't think this holds up simply because whilst the behaviour exhibited perfectly matches the case of adding a constant to a regressor^[$y = \alpha + \beta(x+1)$ can be written as $y = \alpha + \beta + \beta x$ and identifies the 'same' $\beta$ but a different constant.] the relationship isn't linear but logarithmic and therefore not linearly separable^[$log(1 + x) \neq log(x) + 1$ although they're similar if $x$ is small which arguably it is here.]. Therefore I believe if this is the case our variable estimates should be off and whilst they're not absolutely perfect they're pretty close. 

This close but not perfect leads me to my second hypothesis - the data MRW use is subtly different to the data in their appendix. Namely, appendix data are all presented as either integers or to two decimal places but MRW probably used more precise data. This would explain why our variables, which are in logs, are close but not perfect. That is, they're less sensitive to small fluctuations in precision and our constant which is the only factor 'measured' in absolute terms is influenced more by these precision differences.

Finally, it's totally possible that I've completely misinterpreted part of the paper or mis-specified the models. I hope this isn't the case but the two ideas above look like a reasonable starting point to investigate further and will feature in my next post. The former will involve playing around with a lot of different transformations to $s$ and $n+g+d$ whilst the latter will look similar to wild block bootstrapping where the block is the data.

If you spot any mistakes or have any suggestions, please do get in touch.

