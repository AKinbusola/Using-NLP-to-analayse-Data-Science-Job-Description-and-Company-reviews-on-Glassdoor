pacman::p_load("tidyverse", "quanteda", "topicmodels", "stopwords", "tidytext", 
               "caTools", "ROCR", "rpart", "rpart.plot", "car", "Hmisc",
               "quanteda.textplots", "quanteda.textstats", "quanteda.textmodels")


data=read_csv('alldata.csv')

data
corpus_description = corpus(data$description)

summary(corpus_description)

tokens(corpus_description, what = "word")

DFM_desc = corpus_description %>% tokens(what              = "word",
                                          remove_punct      = TRUE,
                                          remove_symbols    = TRUE,
                                          remove_numbers    = TRUE,
                                          remove_url        = TRUE,
                                          remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()

DFM_desc

topfeatures(DFM_desc)


WordCloud_desc = textplot_wordcloud(DFM_desc, 
                                        min_size      = 0.5, 
                                        max_size      = 4, 
                                        min_count     = 2,
                                        max_words     = 500, 
                                        color         = "purple",
                                        rotation      = 0.1, 
                                        random_order  = FALSE, 
                                        random_color  = FALSE,
                                        ordered_color = FALSE, 
                                        labelcolor    = "gold", 
                                        labelsize     = 1.5,
                                        labeloffset   = 0, 
                                        fixed_aspect  = TRUE, 
                                        comparison    = FALSE)







#All State company

data_allstate=read.csv('allstate.xlsx - Sheet1.csv')


corpus_review1 = corpus(data_allstate$Title1)

summary(corpus_review1)

tokens(corpus_review1, what = "word")

DFM_review1 = corpus_review1 %>% tokens(what              = "word",
                                         remove_punct      = TRUE,
                                         remove_symbols    = TRUE,
                                         remove_numbers    = TRUE,
                                         remove_url        = TRUE,
                                         remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()

DFM_review1

topfeatures(DFM_review1)


WordCloud_review1 = textplot_wordcloud(DFM_review1, 
                                    min_size      = 0.5, 
                                    max_size      = 4, 
                                    min_count     = 2,
                                    max_words     = 500, 
                                    color         = "red",
                                    rotation      = 0.1, 
                                    random_order  = FALSE, 
                                    random_color  = FALSE,
                                    ordered_color = FALSE, 
                                    labelcolor    = "gold", 
                                    labelsize     = 1.5,
                                    labeloffset   = 0, 
                                    fixed_aspect  = TRUE, 
                                    comparison    = FALSE)



#Reducing to first 100 reviews


F_DFM_review1 = corpus(data_allstate$Title1[1:100]) %>% tokens(what        = "word",
                                                         remove_punct      = TRUE,
                                                         remove_symbols    = TRUE,
                                                         remove_numbers    = TRUE,
                                                         remove_url        = TRUE,
                                                         remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()




F_DFM_review1 = dfm_remove(F_DFM_review1, c("star",'place','management','claims','service','rep','extremely','always',
                                          'lying','call','calls','center','adjuster','lack','knowledge','availability',
                                          'industry','company','life','job','just','environment','given','agent','staff',
                                          'client','schedule','pay','simply','work','employees','get','can','cowokers','used','hands',
                                          'quit','agents','stars','agency','like','coworkers','within',
                                          'experience','people','sales','workplace','leadership','home','family','team','relationship','allstate',".", ",", "?"))


F_DFM_review1 = as.matrix(F_DFM_review1)
F_DFM_review1 = F_DFM_review1[which(rowSums(F_DFM_review1)>0),]
F_DFM_review1 = as.dfm(F_DFM_review1)

LDA_review1 = LDA(F_DFM_review1, k = 4, control = list(seed = 123))
LDA_review1

### Making the LDA_VEM tidy for further analysis using Tidy package

Tidy_LDA_review1 = tidy(LDA_review1)
summary(Tidy_LDA_review1)

### Provide the term/beat plots for four topics

TopTerms_LDA_review1 = Tidy_LDA_review1 %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

### Visulization

TopTerms_LDA_review1 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  labs(title = "Visualization of Term vs Beta for the Top Four Topics",
       x     = "Terms", 
       y     = "Beta") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + coord_flip()








#Amazon company

data_amazon=read.csv('Amazon.xlsx - Sheet1.csv')

corpus_review2 = corpus(data_amazon$Title1)

summary(corpus_review2)

tokens(corpus_review2, what = "word")

DFM_review2 = corpus_review2 %>% tokens(what              = "word",
                                        remove_punct      = TRUE,
                                        remove_symbols    = TRUE,
                                        remove_numbers    = TRUE,
                                        remove_url        = TRUE,
                                        remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()

DFM_review2

topfeatures(DFM_review2)


WordCloud_review2 = textplot_wordcloud(DFM_review2, 
                                       min_size      = 0.5, 
                                       max_size      = 4, 
                                       min_count     = 2,
                                       max_words     = 500, 
                                       color         = "blue",
                                       rotation      = 0.1, 
                                       random_order  = FALSE, 
                                       random_color  = FALSE,
                                       ordered_color = FALSE, 
                                       labelcolor    = "gold", 
                                       labelsize     = 1.5,
                                       labeloffset   = 0, 
                                       fixed_aspect  = TRUE, 
                                       comparison    = FALSE)


F_DFM_review2 = corpus(data_amazon$Title1[1:100]) %>% tokens(what        = "word",
                                                               remove_punct      = TRUE,
                                                               remove_symbols    = TRUE,
                                                               remove_numbers    = TRUE,
                                                               remove_url        = TRUE,
                                                               remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()





F_DFM_review2 = dfm_remove(F_DFM_review2, c('son','se','lideres','offers','eye','sky','ot','ever','por','los',"amazon",'trabajo','thru','upper','specialist','robotic','provide','phone',
                                            'number','opportunties','mtv','mentors','meet','logistic','imppeza','gratis','global',
                                            'feels','events','de','customers','contracted','communication','caffe','buen','alagre','abente',
                                            'working','many','logistics','college','got','write','women','verbally','treats','thing','students','still','self',
                                            'right','regard','outdoor','investigation','ggvvh','flex','expect','exactly','employed','come',
                                            'brand','assaulted','ambassador','lots','hours','benefits','hr','handle',
                                            'children','overall','mandatory','week','physicality','shifts','small','warehouse','long','befits','eh','gain','however',
                                            'lunch','make','needs','need','opportunities','employee','young','nothing','numbers','one','others','pushed','received','repetitive','retail',
                                            'usually','abiente','aka','alegre','allowed','breathe','cost','every','wage','typical','restroom','quick',
                                            'quick','way','nfe','quickly','past','org','offices','preocupan','use','tu','limpeza','process','poverty','insight','day','nothing','much','moving','member',
                                            'makes','makes','loved','launch','guy','fulltime','flat','environmental','designated','coworkers','continued','consultant','can','build','advance',".", ",", "?"))


F_DFM_review2 = as.matrix(F_DFM_review2)
F_DFM_review2 = F_DFM_review2[which(rowSums(F_DFM_review2)>0),]
F_DFM_review2 = as.dfm(F_DFM_review2)


LDA_review2 = LDA(F_DFM_review2, k = 4, control = list(seed = 123))
LDA_review1

### Making the LDA_VEM tidy for further analysis using Tidy package

Tidy_LDA_review2 = tidy(LDA_review2)
summary(Tidy_LDA_review2)

### Provide the term/beat plots for four topics

TopTerms_LDA_review2 = Tidy_LDA_review2 %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

### Visulization

TopTerms_LDA_review2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  labs(title = "Visualization of Term vs Beta for the Top Four Topics",
       x     = "Terms", 
       y     = "Beta") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + coord_flip()








#BCG company

data_bcg=read.csv('Boston-Consulting-Group.com.xlsx - Sheet1.csv')

corpus_review3 = corpus(data_bcg$Title1)

summary(corpus_review3)

tokens(corpus_review3, what = "word")

DFM_review3 = corpus_review3 %>% tokens(what              = "word",
                                        remove_punct      = TRUE,
                                        remove_symbols    = TRUE,
                                        remove_numbers    = TRUE,
                                        remove_url        = TRUE,
                                        remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()

DFM_review3

topfeatures(DFM_review3)


WordCloud_review3 = textplot_wordcloud(DFM_review3, 
                                       min_size      = 0.5, 
                                       max_size      = 4, 
                                       min_count     = 2,
                                       max_words     = 500, 
                                       color         = "green",
                                       rotation      = 0.1, 
                                       random_order  = FALSE, 
                                       random_color  = FALSE,
                                       ordered_color = FALSE, 
                                       labelcolor    = "gold", 
                                       labelsize     = 1.5,
                                       labeloffset   = 0, 
                                       fixed_aspect  = TRUE, 
                                       comparison    = FALSE)


F_DFM_review3 = corpus(data_bcg$Title1[1:100]) %>% tokens(what        = "word",
                                                             remove_punct      = TRUE,
                                                             remove_symbols    = TRUE,
                                                             remove_numbers    = TRUE,
                                                             remove_url        = TRUE,
                                                             remove_separators = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()


F_DFM_review3 = dfm_remove(DFM_review3, c('place','work','company','workplace','people','culture',
                                            'career','environment','high','bog','learn','benefits','experience',
                                            'overall','management','bcg','fast','start','pay','team','recommend',
                                            'last','development','balance','bisness','worked','grow','job','top',
                                            'staff','business','learning','curve','advancement','room','growth','review',
                                            'hours','client','skills','one','new','develop','companies','way','nfe','quickly','past','org','offices','preocupan','use','tu','limpeza','process','poverty','insight','day','nothing','much','moving','member',
                                          'makes','makes','loved','launch','guy','fulltime','flat','environmental','designated','coworkers','continued','consultant','can','build','advance',".", ",", "?"))


F_DFM_review3 = as.matrix(F_DFM_review3)
F_DFM_review3 = F_DFM_review3[which(rowSums(F_DFM_review3)>0),]
F_DFM_review3 = as.dfm(F_DFM_review3)

LDA_review3 = LDA(F_DFM_review3, k = 4, control = list(seed = 123))
LDA_review3

### Making the LDA_VEM tidy for further analysis using Tidy package

Tidy_LDA_review3 = tidy(LDA_review3)
summary(Tidy_LDA_review3)

### Provide the term/beat plots for four topics

TopTerms_LDA_review3 = Tidy_LDA_review3 %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

### Visulization

TopTerms_LDA_review3 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  labs(title = "Visualization of Term vs Beta for the Top Four Topics",
       x     = "Terms", 
       y     = "Beta") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + coord_flip()

