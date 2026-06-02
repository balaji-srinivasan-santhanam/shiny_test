
source("global_posit.R", local = TRUE)

tcga_key = list('AML'='aml', 'bladder'='blca', 'breast'='brca', 'cervical'='cesc', 
                'colon'='coad', 'esophageal'='esca', 'GBM'='gbm', 'glioma'='glm', 
                'head_neck'='hnsc', 'kidney_cc'='kicc', 'kidney_ch' = 'kich', 
                'kidney_pa' = 'kipa', 'liver'='lihc', 'lung_ad'='luad',
                'lung_sq'='lusq', 'melanoma'='skcm','ovarian'='ovsc', 'pancreatic'='paad', 
                'paraganglioma'='pcpg', 'prostate'='prad', 'rectal'='read', 'sarcoma'='sarc', 
                'stomach'='stad', 'testicular'='tgct', 'thymoma'='thym', 'thyroid'='thca', 
                'uterine_endometrial'='ucec')

dis_key = list('AML'='tcga_aml', 'bladder'='tcga_blca', 'breast'='tcga_brca', 'cervical'='tcga_cesc', 
                'colon'='tcga_coad', 'esophageal'='tcga_esca', 'GBM'='tcga_gbm', 'glioma'='tcga_glm', 
                'head_neck'='tcga_hnsc', 'kidney_cc'='tcga_kicc', 'kidney_ch' = 'tcga_kich', 
                'kidney_pa' = 'tcga_kipa', 'liver'='tcga_lihc', 'lung_ad'='tcga_luad',
                'lung_sq'='tcga_lusq', 'melanoma'='tcga_skcm','ovarian'='tcga_ovsc', 'pancreatic'='tcga_paad', 
                'paraganglioma'='tcga_pcpg', 'prostate'='tcga_prad', 'rectal'='tcga_read', 'sarcoma'='tcga_sarc', 
                'stomach'='tcga_stad', 'testicular'='tcga_tgct', 'thymoma'='tcga_thym', 'thyroid'='tcga_thca', 
                'uterine_endometrial'='tcga_ucec', 'metabric_breast' = 'metabric_mbbreast')

col_surv = rev(c(rgb(0.86, 0.2, 0.3, 0.75), rgb(0, 0.5, 1, 0.75)))
quant_th = 0.1 ; MI_bins = 10


label_clinicalData <- function(clinical_data) {
  z = clinical_data
  zz2 = NULL
  ix_ = which(z$MPS != 0)
  if (length(ix_) > 25) {
    zz = z[ix_,]
    zz_p = zz[which(zz$MPS > 0),] ; zz_p$group = 'MPS+'
    zz_p = zz_p[sort(abs(zz_p$MPS), decreasing=T, index.return=T)$ix,]
    zz_n = zz[which(zz$MPS < 0),] ; zz_n$group = 'MPS-'
    zz_n = zz_n[sort(abs(zz_n$MPS), decreasing=T, index.return=T)$ix,]
    zz2 = list(zz_p, zz_n)
  }
  val_ret = zz2
}

calculateMI_v2 <- function(x, y) {
        #x = cMatrix[,1] ; y = cMatrix[,2] ; # = rowSums(cMatrix)
        #x[is.na(x)] = 0 ; y[is.na(y)] = 0
        rs_cMatrix = x + y ; num_genes = sum(rs_cMatrix)
        log_t1 = log((num_genes*x)/(sum(x) * rs_cMatrix)) ; log_t1[is.infinite(log_t1)] = 0
        log_t2 = log((num_genes*y)/(sum(y) * rs_cMatrix)) ; log_t2[is.infinite(log_t2)] = 0
        val_ret = sum((1/num_genes)* ((x)*log_t1 + (y)*log_t2))
}
#MPS_thresh = 0
calculate_mps_new <- function(clin_df, exp_con, exp_dis, user_genes, module_name, module_id) {
    number_bins = 10 ; pval_thresh = 0.01 ; num_rand = 10000 ; MPS_thresh = 0
    inp_g = user_genes
    g_set = intersect(common_genes, intersect(rownames(exp_dis), rownames(exp_con)))
    s_set = intersect(colnames(exp_con), colnames(exp_dis))
    con_g = exp_con[g_set, s_set] ; dis_g = exp_dis[g_set, s_set]
    genes_in_mod = intersect(inp_g, g_set) ; div_f = as.numeric(length(genes_in_mod))
    set.seed(108)
    total_genes = length(g_set)
    total_genes_bin = rmultinom(1, total_genes, rep((1/number_bins), number_bins))
    num_in_path = div_f
    path_genes_bin = rmultinom(num_rand, num_in_path, rep((1/number_bins), number_bins))
    c_vec_r = path_genes_bin ; nc_vec_r = total_genes_bin[,1] - c_vec_r
    v_rand = sapply(seq(num_rand), function(i) calculateMI_v2(c_vec_r[,i], nc_vec_r[,i]))
    mu_r = mean(v_rand) ; sd_r = sd(v_rand)
    v_rand_q = as.numeric(quantile(v_rand, (1-pval_thresh)))
    len_rand = length(v_rand)

    Nrow = dim(con_g)[1] ; per_bin = round(Nrow/number_bins)
    registerDoMC(cores=4)
    out_list <- foreach(sam_ = s_set) %dopar% {
        vv = con_g[sam_] ; vv$bin = 0 ; vv[genes_in_mod,]$bin = 1 ; sign_fact = sign(cor(vv)[1,2])
        vv = dis_g[sam_]
        l_ = lapply(seq(number_bins), function(i) rownames(vv)[vv[sam_][,1] == i])
        c_vec = unlist(lapply(lapply(l_, intersect, genes_in_mod), length))
        nc_vec= as.numeric(sapply(l_, length) - c_vec )
        mi_ = calculateMI_v2(c_vec, nc_vec) ; mi_sign = sign_fact*mi_
    }
    rm(con_g) ; rm(dis_g)
    mi_vec = unlist(out_list) ; abs_mi = abs(mi_vec) ; ix_0 = which(abs_mi < v_rand_q)
    v_z = (abs_mi - mu_r)/sd_r ; v_z[v_z < 0] = 0 ; v_z[is.na(v_z)] = 0 ; v_z[ix_0] = 0
    v_z = v_z*sign(mi_vec)
    mod_mps = data.frame(SAMPLE_ID = s_set, MPS = v_z)
    mod_mps$module_id = module_id ; mod_mps$module_name = module_name
    mod_mps$num_genes = length(genes_in_mod)
    mps_df = merge(mod_mps, clin_df)
    val_ret = mps_df
}

getSurv_shiny <- function(all_clin_df, surv_type = 'OS', rand_iter = 1000, samp_name = 'MPS+', ctrl_name = 'MPS-') {
  library('survival')
  all_clin = all_clin_df
  s_name = samp_name #; if(s_name == 'positive') { s_name = 'MPS+' }
  c_name = ctrl_name #; if(c_name == 'negative') { c_name = 'MPS–' }
  if (surv_type == 'OS') { all_clin$mod_censor_time = as.numeric(all_clin$OS.time) ; all_clin$death_event_binary = as.numeric(all_clin$OS) }
  if (surv_type =='PFI') { all_clin$mod_censor_time =as.numeric(all_clin$PFI.time) ; all_clin$death_event_binary = as.numeric(all_clin$PFI) }
  len_s = sum(all_clin$group == samp_name) ; len_c = sum(all_clin$group == ctrl_name)
  s_all = survfit(Surv(as.numeric(all_clin$mod_censor_time), all_clin$death_event_binary) ~ 1)
  ss = Surv(as.numeric(as.character(all_clin$mod_censor_time)),all_clin$death_event_binary)
  val_ret = NA
  pv = s_fit = pv_cox = hz_ratio = s_coxph = z = c_ix = NA
  if (! (sum(is.na(ss)) == length(is.na(ss)))) {
    s_fit = survfit(Surv(mod_censor_time, death_event_binary) ~ group, data = all_clin)
    s_diff = tryCatch({ 
      survdiff(Surv(mod_censor_time, death_event_binary) ~ group, data = all_clin)
    }, error = function(err) { list(chisq=NA, n=dim(all_clin)[1]) } )
    pv <- ifelse(is.na(s_diff),1,(round(1 - pchisq(s_diff$chisq, length(s_diff$n) - 1),50)))[[1]]
    
    s_coxph = coxph(Surv(mod_censor_time, death_event_binary) ~ group, data = all_clin)
    if (! is.na(s_coxph[['coefficients']])) { 
      s_c = summary(s_coxph)
      hz_ratio = as.numeric(data.frame(s_c$coefficients)['exp.coef.'])
      pv_cox = as.numeric(data.frame(s_c$coefficients)['Pr...z..'])
      z = as.numeric(data.frame(s_c$coefficients)['z'])
      c_ix = as.numeric(summary(s_coxph)$concordance['C'])
    }
    
    pv_r = c()
    #all_clin_r = all_clin ; prob_v = as.numeric(prop.table(table(all_clin$group)))
    #grp_r = rand_iter*sample(c(s_name, c_name), dim(all_clin)[1], replace=T, prob=prob_v)
    
    for(r_i in seq(rand_iter)) {
      all_clin_r = all_clin ; prob_v = as.numeric(prop.table(table(all_clin$group)))
      all_clin_r$group = sample(c(s_name, c_name), dim(all_clin)[1], replace=T, prob=prob_v)
      s1_r = tryCatch({ 
        survdiff(Surv(mod_censor_time, death_event_binary) ~ group, data = all_clin_r)
      }, error = function(err) { list(chisq=NA, n=dim(all_clin_r)[1]) } )
      pv_r = c(pv_r, round(1 - pchisq(s1_r$chisq, length(s1_r$n) - 1),50))
    }
    fp = (sum(pv_r < pv)/rand_iter)
    median_surv = summary(s_fit)$table[,'median']
    #        med_ab = round(as.numeric(median_surv[grep(samp_name,names(median_surv))]),1)
    #        med_re = round(as.numeric(median_surv[grep(ctrl_name,names(median_surv))]),1)
    med_ab = round(as.numeric(median_surv[paste('group=',s_name,sep='')]),1)
    med_re = round(as.numeric(median_surv[paste('group=',c_name,sep='')]),1)
    m_sub = paste(s_name,' ', len_s, '(', med_ab,' mo)',
                  '| ', c_name,' ', len_c, '(', med_re, ' mo)', sep='')
    
    out_df = data.frame(pval_surv = pv, pval_cox = pv_cox, C_index = c_ix, z_cox = z,HZR_cox = hz_ratio, info_survival = m_sub)
    
    #        s_dt = survfit(Surv(mod_censor_time, death_event_binary) ~ (group+disease_type), data = all_clin)
    #        s1_dt = tryCatch({ 
    #            survdiff(Surv(as.numeric(as.character(all_clin$mod_censor_time)),all_clin$death_event_binary) ~ all_clin$group + all_clin$disease_type)
    #            }, error = function(err) { list(chisq=NA, n=dim(all_clin)[1]) } )
    #        pv_distype <- ifelse(is.na(s1),1,(round(1 - pchisq(s1$chisq, length(s1$n) - 1),50)))[[1]]
    val_ret = list(KM_pv = pv, KM_fit = s_fit, clin_data = all_clin, cox_pv = pv_cox, hzr = hz_ratio, 
                   cox_fit = s_coxph, avg_fit = s_all, rand_pv = pv_r, z=z,
                   concordance = c_ix, out_df = out_df, fdr = fp)
  }
}

plot_Surv <- function(clin_data) {
    all_clin = clin_data
    dis_ = as.character(unique(all_clin$cohort)) ; module_id = as.character(unique(all_clin$module_id))
    #collection = as.character(unique(all_clin$collection))
    #d_module = readRDS(paste(parent_dir, 'shiny_test/data/module_info/', path_id, '_rds', sep=''))
    name_module = as.character(unique(all_clin$module_name)) ; name_module_trim = strtrim(name_module, 14)
    #genes_in_module = intersect(as.character(gene2module$Approved.Symbol), unlist(strsplit(as.character(clin_data$genes_in_mod), '\\|')))
    #genes_in_module = unique(unlist(strsplit(as.character(clin_data$genes_in_mod), '\\|')))
    num_genes_in_module = unique(all_clin$num_genes)
    pl_o = pl_p = nullGrob()
    if (sum(table(all_clin$group) > 20) == length(unique(all_clin$group))) {
        surv_ovs = getSurv_shiny(all_clin, 'OS',1) ; surv_pfs = getSurv_shiny(all_clin, 'PFI',1)
        fdr_ovs = as.numeric(surv_ovs[[12]]) ; fdr_pfs = as.numeric(surv_pfs[[12]])
        if (fdr_ovs == 0) { f_ovs = '1e-3' } ; if (fdr_ovs != 0) { f_ovs = formatC(fdr_ovs,format='e',digits=1) }
        if (fdr_pfs == 0) { f_pfs = '1e-3' } ; if (fdr_pfs != 0) { f_pfs = formatC(fdr_pfs,format='e',digits=1) }

        m_ovs= paste(dis_, 'p=', formatC(as.numeric(surv_ovs[[11]]$pval_surv),format='e',digits=1), 
                     ' | fdr<', f_ovs,' | HR=', signif(as.numeric(surv_ovs[[11]]$HZR_cox), 2))
        m_ovs= paste(dis_, 'p=', formatC(as.numeric(surv_ovs[[11]]$pval_surv),format='e',digits=1), 
                     ' | HR=', signif(as.numeric(surv_ovs[[11]]$HZR_cox), 2))
        m_sub_ovs = paste('OVS (', num_genes_in_module,' genes)  ',name_module_trim, sep='')
        m_pfs= paste(dis_, 'p=', formatC(as.numeric(surv_pfs[[11]]$pval_surv),format='e',digits=1), 
                     ' | fdr<', f_pfs,' | HR=', signif(as.numeric(surv_pfs[[11]]$HZR_cox), 2))
        m_pfs= paste(dis_, 'p=', formatC(as.numeric(surv_pfs[[11]]$pval_surv),format='e',digits=1), 
                     ' | HR=', signif(as.numeric(surv_pfs[[11]]$HZR_cox), 2))
        m_sub_pfs = paste('PFS (', num_genes_in_module,' genes). ' ,name_module_trim, sep='')
       
        pl_ovs = ggsurvplot(surv_ovs[[2]], surv_ovs[[3]], pval=F, title=m_ovs, font.title=12, censor.shape=124,censor.size=2,
                            subtitle=m_sub_ovs, font.subtitle=10,surv.median.line='hv', palette =col_surv, risk.table=F)
    
        pl_pfs = ggsurvplot(surv_pfs[[2]], surv_pfs[[3]], pval=F, title=m_pfs, font.title=12, censor.shape=124,censor.size=2,
                            subtitle=m_sub_pfs, font.subtitle=10,surv.median.line='hv', palette =col_surv, risk.table=F)
        pl_o = pl_ovs$plot ; pl_p = pl_pfs$plot ; if (dis_ == 'AML') { pl_p = nullGrob() }
    }
    val_ret = list(pl_o, pl_p)
}

get_commonHistopath <- function(disease_name) {
    dis_ = disease_name
    hist_v = hist_v_ori = c('histological_type', 'coarse_pathologic_stage', 'age_category')
    if (dis_ == 'breast') { hist_v = union(c('HR_category', 'HER2_category', 'TN_category'), hist_v_ori) }

    if (dis_ == 'prostate') { hist_v = union(c('gleason_score_category','PSA_value'), hist_v_ori) }
    
    if (dis_ == 'AML') { hist_v = union(c('mol_test_status'), hist_v_ori) }

    if (dis_ == 'head_neck') { hist_v = union(c('HPV_status'), hist_v_ori) }

    if (dis_ == 'cervical') {  hist_v = union(c('HPV_status'), hist_v_ori) }

    if (dis_ == 'colon') { hist_v = union(c('MSI_status','tumor_side'), hist_v_ori) }

    val_ret = hist_v
}

plot_Histopath <- function(clin_data) {
    all_clin = clin_data
    dis_ = as.character(unique(all_clin$cohort)) ; path_id = as.character(unique(all_clin$module_id))
    hist_v = get_commonHistopath(dis_) #c('histological_type', 'coarse_pathologic_stage', 'age_groups')
    dis_hist = all_clin[,c(hist_v, 'group')]
    pl_hist = nullGrob()
    if (sum(table(dis_hist$group) > 20) == length(unique(dis_hist$group))) {
        pl_hist = list()
        for(h_v in sort(hist_v)) {
            if (h_v != 'PSA_value') {
                tmp_h = dis_hist[,union('group', h_v)] ; colnames(tmp_h)[which(colnames(tmp_h) == h_v)] = 'var'
                t_ = table(as.character(tmp_h$var)) ; tmp_h$var = as.character(tmp_h$var)
                if (length(t_) > 1) {
                    for(i in names(t_)) { ix_ = which(tmp_h$var == i) ; tmp_h$var[ix_] = paste(i, ' (', as.numeric(t_[i]), ')',sep='') }
                    pl_ = ggplot(tmp_h, aes(var, fill=group)) + geom_bar(stat = "count",position = "fill", width=0.25) + 
                            scale_y_continuous(labels=scales::percent) + theme_bw() + xlab('') + #coord_flip() +
                            scale_fill_manual(values=col_surv) + ggtitle(gsub('_', ' ', h_v)) + ylab('percent') + coord_flip()#+ facet_wrap(~ hist_var, scales = "free_x")
                    pl_hist[[h_v]] = pl_
                }
            }
            if (h_v == 'PSA_value') {
                tmp_h = dis_hist[,union('group', h_v)] ; colnames(tmp_h)[which(colnames(tmp_h) == h_v)] = 'var'
                pl_ = ggplot(tmp_h, aes(group, log2(var), fill=group)) + theme_bw() + xlab('') + ylab(paste('log2(', gsub('_', ' ', h_v), ') a.u')) + 
                        geom_violin(alpha=0.5, draw_quantiles=c(0.25, 0.5, 0.75)) + scale_fill_manual(values=col_surv) + 
                        ggtitle(gsub('_', ' ', h_v)) + coord_flip()
                pl_hist[[h_v]] = pl_
            }
        }
    }
    val_ret = pl_hist
}


plot_Histopath_multivariate <- function(clin_data) {#1 = 'group', var2 = 'histology') {
    all_clin = clin_data
    dis_ = as.character(unique(all_clin$cohort))
    var_vec = c('group', get_commonHistopath(dis_))

    ix_rm = c() ; var_vec_ex = c()
    for(v_v in var_vec) {
        all_clin[v_v][,1] = as.character(all_clin[v_v][,1])
        ix_na = which(is.na(all_clin[v_v][,1]))
        if (dim(all_clin)[1] - length(ix_na) < 30) { var_vec_ex = c(var_vec_ex, v_v) }
        if (dim(all_clin)[1] - length(ix_na) >= 30) {
            ix_rm = union(ix_rm, which(is.na(all_clin[v_v][,1]))) #; print(ix_rm) ; print(v_v)
        }
        t_ = table(as.character(all_clin[v_v][,1])) ; cat_n = names(which(t_ <= 10))
        if ((length(t_) - length(cat_n)) > 1 & length(cat_n) >= 1) {
            for(n_ in cat_n) { ix_rm = union(ix_rm, which(all_clin[v_v][,1] == n_)) }
        }
        if ((length(t_) - length(cat_n)) <= 1) { var_vec_ex = c(var_vec_ex, v_v) }
    }
    var_vec = setdiff(var_vec, var_vec_ex) ; all_clin = all_clin[setdiff(seq(dim(all_clin)[1]), ix_rm), ]
    #all_clin$mod_censor_time = all_clin$mod_censor_time/30
    all_clin_num = all_clin ; for(v_v in var_vec) { all_clin_num[v_v][,1] = as.numeric(factor(all_clin_num[v_v][,1])) }
    ss_ovs = Surv(as.numeric(as.character(all_clin$OS.time)),all_clin$OS)
    ss_pfs = Surv(as.numeric(as.character(all_clin$PFI.time)),all_clin$PFI)
    val_ret = list(NA, NA, NA) ; p_for_ovs = p_for_pfs = nullGrob()
    if (! (sum(is.na(ss_ovs)) == length(is.na(ss_ovs)))) {
        su_ovs = Surv((all_clin$OS.time), (all_clin$OS))
        s_cox_ovs = coxph(as.formula(paste('su_ovs ~', paste(var_vec, collapse='+'))), data=all_clin_num)
        p_for_ovs = ggforest(s_cox_ovs,all_clin_num, main='OS',refLabel = "reference")
        
    }
    if (! (sum(is.na(ss_pfs)) == length(is.na(ss_pfs)))) {
        su_pfs = Surv((all_clin$PFI.time), (all_clin$PFI))
        s_cox_pfs = coxph(as.formula(paste('su_pfs ~', paste(var_vec, collapse='+'))), data=all_clin_num)
        p_for_pfs = ggforest(s_cox_pfs,all_clin_num, main='PFI',refLabel = "reference")
        if (dis_ == 'AML') { p_for_pfs = nullGrob() }
    }
    val_ret = list(p_for_ovs, p_for_pfs)
}


plot_Histopath_subset <- function(clin_data) {#1 = 'group', var2 = 'histology') {
    all_clin = clin_data# ; rownames(all_clin) = all_clin$SAMPLE_ID
    dis_ = as.character(unique(all_clin$cohort))
    var_vec = c('group', get_commonHistopath(dis_)) #c('histological_type', 'coarse_pathologic_stage', 'age_groups')

    ix_rm = c() ; var_vec_ex = c()
    for(v_v in var_vec) {
        all_clin[v_v][,1] = as.character(all_clin[v_v][,1])
        ix_na = which(is.na(all_clin[v_v][,1]))
        if (dim(all_clin)[1] - length(ix_na) < 30) { var_vec_ex = c(var_vec_ex, v_v) }
        if (dim(all_clin)[1] - length(ix_na) >= 30) {
            ix_rm = union(ix_rm, which(is.na(all_clin[v_v][,1]))) #; print(ix_rm) ; print(v_v)
        }
        t_ = table(as.character(all_clin[v_v][,1])) ; cat_n = names(which(t_ <= 10))
        if ((length(t_) - length(cat_n)) > 1 & length(cat_n) >= 1) {
            for(n_ in cat_n) { ix_rm = union(ix_rm, which(all_clin[v_v][,1] == n_)) }
        }
        if ((length(t_) - length(cat_n)) <= 1) { var_vec_ex = c(var_vec_ex, v_v) }
    }
    var_vec = setdiff(var_vec, var_vec_ex) ; all_clin = all_clin[setdiff(seq(dim(all_clin)[1]), ix_rm), ]
    c_ = 1 ; pl_list_ovs = pl_list_pfs = list()
    for(v_v in setdiff(var_vec, 'group')) {
        var_cat = as.character(unique(all_clin[v_v][,1]))
        for(v_c in var_cat) {
            all_clin_var = all_clin[which(all_clin[v_v][,1] == v_c),]
            pl_o = pl_p = nullGrob()
            if (sum(table(all_clin_var$group) > 5) == length(unique(all_clin_var$group))) {
                surv_ovs = getSurv_shiny(all_clin_var, 'OS',1) ; surv_pfs = getSurv_shiny(all_clin_var, 'PFI',1)
                m_ovs= paste(v_v,  ' | p=', formatC(as.numeric(surv_ovs[[11]]$pval_surv),format='e',digits=1), 
                         ' | HR=', signif(as.numeric(surv_ovs[[11]]$HZR_cox), 2))
                m_sub_ovs = paste('OVS (', v_c,')', sep='')
                m_pfs= paste(v_v, ' | p=', formatC(as.numeric(surv_pfs[[11]]$pval_surv),format='e',digits=1), 
                         ' | HR=', signif(as.numeric(surv_pfs[[11]]$HZR_cox), 2))
                m_sub_pfs = paste('PFS (', v_c,')', sep='')
                pl_ovs = ggsurvplot(surv_ovs[[2]], surv_ovs[[3]], pval=F, title=m_ovs, font.title=12, censor.shape=124,censor.size=2,
                            subtitle=m_sub_ovs, font.subtitle=10,surv.median.line='hv', palette =col_surv, risk.table=F)
                pl_pfs = ggsurvplot(surv_pfs[[2]], surv_pfs[[3]], pval=F, title=m_pfs, font.title=12, censor.shape=124,censor.size=2,
                            subtitle=m_sub_pfs, font.subtitle=10,surv.median.line='hv', palette =col_surv, risk.table=F)
                pl_o = pl_ovs$plot ; pl_p = pl_pfs$plot ; pl_list_ovs[[c_]] = pl_o ; pl_list_pfs[[c_]] = pl_p
                if (dis_ == 'AML') { pl_list_pfs[[c_]] = nullGrob() } ; c_ = c_ + 1
            }
        }
    }
    val_ret = list(pl_list_ovs, pl_list_pfs)         
}



ui <- fluidPage(
        title = 'icamp',
    #fluidRow(
        titlePanel("Inferring Clinical Associations of Module Perturbations"),
    
        sidebarPanel(
            selectInput("disease_name", "patient cohort",
                      c("pancreatic", "AML", "bladder", "breast", "cervical", "colon", "esophageal", "GBM",
                        "glioma", "head_neck", "kidney_cc", "kidney_ch", "kidney_pa",
                        "liver", "lung_ad", "lung_sq",
                        "melanoma", "ovarian", "pancreatic", "paraganglioma", "prostate",
                        "rectal", "sarcoma", "stomach", "testicular", "thymoma", "thyroid","uterine_endometrial"),
                      ),
                       
            selectInput("specify_module", "explore modules",
                      c(existingModule = "existing", newModule = "new")
                      ),

            conditionalPanel(
                condition = "input.specify_module == 'existing'",
                selectizeInput("rbp_select", "Search RBP / Module Name:", 
               choices = NULL, 
               options = list(
                 placeholder = 'Type ...',
                 maxOptions = 10,       # Limits visible results for a cleaner UI
                 minLength = 2,         # Only starts searching after 2 characters
                 server = TRUE          # Essential for performance
               )),
              helpText("Retrieves precomputed modules")),

            conditionalPanel(
        condition = "input.specify_module == 'new'",
        textInput(inputId = 'new_name', label = 'Name of module', value = 'my_module'),
        fileInput("gene_file", "Upload Gene List (.txt)", accept = ".txt"),
        helpText("Upload a list of Gene Symbols (one per line)."),
        actionButton("run_icamp", "Compute survival curves", class = "btn-success")
      ),
      
      hr(),

      textInput(inputId = 'num_pat',
                label = 'Maximum number of patients per group',
                value = 50)

            # sliderInput(inputId = 'MPS_thresh',
            # label='MPS threshold:',
            # min=0,
            # max=10,
            # value=2),
#     textInput(inputId='module_identifier',
#           label='Module identifier',
#           value='FIRE1KBup_397')
#     textInput(inputId = 'disease_name',
#           label='cancer',
#           value='AML')
      ),

            mainPanel(

                tabsetPanel(type = "tabs",
                    #tabPanel("module info (genes)", textOutput('module_info'), plotOutput(outputId='geneEnrichPlot')),
                    #tabPanel("module info (genes)", htmlOutput('module_info'), plotOutput(outputId='volcanoPlot'),
                    #                                plotOutput(outputId='volcanoPlot', click='plot_click'), verbatimTextOutput('volc_info')),
                    #tabPanel("module info (genes)", htmlOutput('module_info'), #plotOutput(outputId='volcanoPlot')),
                    #                                plotOutput(outputId='geneEnrichPlot', click='plot_click'), verbatimTextOutput('volc_info')),
                    tabPanel("clinical data", htmlOutput('table_info'), tableOutput('table'), downloadButton("downloadData", "download table")),
                    tabPanel("survival", column(12, plotOutput(outputId='survPlot'))),
                    tabPanel("histopathology", column(12, plotOutput(outputId='histPlot'))),
                    tabPanel("histopathology (multivariate)", column(12, plotOutput(outputId='histPlot2'))),
                    tabPanel("histopathology (subset: OVS)", column(12, plotOutput(outputId='histPlot3'))),
                    tabPanel("histopathology (subset: PFS)", column(12, plotOutput(outputId='histPlot4'))),
                    #tabPanel("SNVs and CNAs", column(12, plotOutput(outputId='SNVCNAPlot'))),
                    #tabPanel("immune", column(12, plotOutput(outputId='immPlot')))
                    
                )

            )

        #),
    #fluidRow(
        #    title = "Inferring Clinical Associations of Module Perturbations (iCAMP)",
        #    column(12, tableOutput('table'), downloadButton("downloadData", "download table")
        #        #plotOutput(outputId='survPlot')
        #    
        #    )
        #)
    )



server <- function(input, output, session) {

    updateSelectizeInput(session, "rbp_select", choices = module_rbps$module_name, server = TRUE)

    existing_module_data <- reactive({
      req(input$specify_module == "existing", input$rbp_select)
    
      # 1. Map module_name to MID
      mid <- as.character(module_rbps[which(module_rbps$module_name == input$rbp_select),]$module_id)
      #message(mid)
      rbp_name <- as.character(module_rbps[which(module_rbps$module_name == input$rbp_select),]$RBP)
      num_genes = as.numeric(module_rbps[which(module_rbps$module_name == input$rbp_select),]$num_genes)
    
      dis_name = input$disease_name
      dis_name_full = dis_key[[dis_name]]#dis_key[[which(names(dis_key) == dis_name)]]
      coh_folder = sapply(strsplit(dis_name_full, '_'), '[', 1) ; dis_folder = toupper(sapply(strsplit(dis_name_full, '_'), '[', 2))

      # 2. Construct GCS Path: data/cohort/tcga/BRCA/module/MID.RDS
      path <- paste0("data/cohort/", coh_folder, "/", dis_folder, "/modules/", mid, ".RDS")
      
      #message("--- GCS ATTEMPT ---")
      #message("Module Path: ", path)
      #message("Clinical Path: ", clin_path)
      
      # 3. Pull from GCS
      mod_mps = get_gcs_rds(path) ; mod_mps$MPS = mod_mps[mid][,1]
      mod_mps$module_id = mid
      mod_mps$module_name = input$rbp_select ; mod_mps$num_genes = num_genes
      mod_mps$RBP = rbp_name

    
      clin_df = get_gcs_rds(paste0("data/cohort/", coh_folder, "/", dis_folder, "/clinical.RDS"))
      mps_df = merge(mod_mps, clin_df)
      mps_df
    })

    new_module_data <- eventReactive(input$run_icamp, {
      req(input$specify_module == "new", input$gene_file)
    
      withProgress(message = 'Running iCAMP...', value = 0, {
      # 1. Load Expression & other inputs needed for calculation
      dis_name = input$disease_name
      dis_name_full = dis_key[[which(names(dis_key) == dis_name)]]
      coh_folder = sapply(strsplit(dis_name_full, '_'), '[', 1) ; dis_folder = toupper(sapply(strsplit(dis_name_full, '_'), '[', 2))
      setProgress(0.2, detail = "Fetching relevant datasets...")
      # 2. Construct GCS Path: data/cohort/tcga/BRCA/module/MID.RDS
      path_con <- paste0("data/cohort/", coh_folder, "/", dis_folder, "/expression_con.RDS")
      path_dis <- paste0("data/cohort/", coh_folder, "/", dis_folder, "/expression_dis.RDS")
    
      exp_con <- get_gcs_rds(path_con)
      exp_dis <- get_gcs_rds(path_dis)
      clin_df = get_gcs_rds(paste0("data/cohort/", coh_folder, "/", dis_folder, "/clinical.RDS"))

      # 2. Read User Genes
      setProgress(0.4, detail = "Parsing uploaded gene list...")
      user_genes <- readLines(input$gene_file$datapath)
    
      # 3. Pipeline Calculation (Example: simple Z-score or mean)
      # This is where your specific math happens
      mid = paste('mod_', gsub('\\-', '', gsub(' ', '_', gsub('\\:', '_', as.character(Sys.time())))) , sep='')
      mod_name = input$new_name ; if (mod_name == '') { mod_name = mid }
      message("Computing score for ", length(user_genes), " genes in ", mod_name)
      
      setProgress(0.6, detail = "Running 10,000 permutations (this may take a minute)...")
      # Placeholder: Assuming the result returns a dataframe compatible with your plots
      res = calculate_mps_new(clin_df, exp_con, exp_dis, user_genes, mod_name, mid)
      setProgress(1, detail = "Computation complete!")
      return(res)
      })
    })



    clinDataInput <- reactive({
      if (input$specify_module == "existing") { req(input$rbp_select) ; clin_inp_ = label_clinicalData(existing_module_data()) }
      if (input$specify_module == "new") { clin_inp_ = label_clinicalData(new_module_data()) }
      num_pat  = as.numeric(input$num_pat)
      clin_inp = NULL
      if (! is.null(clin_inp_)) {
        pat_p = clin_inp_[[1]] ; pat_n = clin_inp_[[2]]
        clin_inp = rbind(pat_p[1:min(c(dim(pat_p)[1], num_pat)),], pat_n[1:min(c(dim(pat_n)[1], num_pat)),])
      }
      clin_inp
    })


    # clinDataInput <- reactive({
    #     zs_thresh = 0 ; MPS_thresh = 0 ; fold_CV = 3
    #     #MPS_thresh = input$MPS_thresh ; fold_CV = 3
    #     #zs_thresh = 0
    #     path_id = input$module_identifier
    #     disease_name = input$disease_name
    #     #get_clinicalParam(path_id, disease_name, zs_thresh)
    #     get_clinicalParam(module_identifier = path_id, collection = 'tcga', disease_name = disease_name, MPS_thresh = zs_thresh)
    # })


    #geneDataInput <- reactive({ get_geneExp_info(clinDataInput()) })

    output$table_info <- renderText({ 
        x = clinDataInput()
        x = paste('<b> number of patients: ', dim(x)[1], "<br>", 'MPS+ ', sum(x$group == 'MPS+'), ' | MPS- ', sum(x$group == 'MPS-'),"<br>","<br>", "</b>")
    })
    
    #output$module_info <- renderText({ 
        #pl = plot_moduleGenes(clinDataInput())
        #x = paste('number of genes: ', pl[[2]], ' | name of module:', pl[[3]])
        #x = paste('<b> number of genes: ', pl[[2]], "<br>", 'name of module:', pl[[3]], "<br>")
    #})
    
    output$table <- renderTable({
        x = clinDataInput()
        x[1:2,]
        #print(x[1:3,])
        })
    output$downloadData <- downloadHandler(

        filename = function() { paste('file_',gsub(':','-', gsub('-','', gsub(' ', '_',Sys.time() ))),'.csv',sep='') },
        content = function(file) { write.csv(clinDataInput(), file, row.names=FALSE)}
        #print(x[1:3,])
    )
    output$survPlot <- renderPlot({
        pl = plot_Surv(clinDataInput())
        grid.arrange(pl[[1]], pl[[2]], nullGrob(), nullGrob(), ncol=2)
    } , height = 600, width = 600)
    
    output$histPlot <- renderPlot({ 
        pl = plot_Histopath(clinDataInput()) ; n_col = ceiling(sqrt(length(pl)))
        grid.arrange(grobs=pl, ncol=n_col)
        }, height = 300, width = 700)
    
    #output$SNVCNAPlot <- renderPlot({ 
    #    pl = plot_topSNVs_CNAs(clinDataInput())
    #    grid.arrange(pl[[1]], pl[[2]], nullGrob(), nullGrob(), ncol=2) 
    #    }, height = 600, width = 600)

    #output$immPlot <- renderPlot({ pl = plot_Immune(clinDataInput()) ; plot(pl[[1]]) }, height = 300, width = 500)
    
    #output$volcanoPlot <- renderPlot({ pl = plot_geneExp(get_geneExp_info(clinDataInput())) ; plot(pl[[1]]) }, height = 300, width = 300)
    output$histPlot2 <-   renderPlot({ 
        pl = plot_Histopath_multivariate(clinDataInput())
        grid.arrange(pl[[1]], pl[[2]], nullGrob(), nullGrob(), ncol=2) 
    }, height = 800, width = 900)
    output$histPlot3 <-   renderPlot({ 
        pl = plot_Histopath_subset(clinDataInput())
        p_ = pl[[1]]
        n_col = ceiling(sqrt(length(p_)))
        if (n_col == 0) { grid.arrange(nullGrob()) }
        if (n_col != 0) {grid.arrange(grobs = p_, ncol=n_col) }
    }, height = 800, width = 900)
    output$histPlot4 <-   renderPlot({ 
        pl = plot_Histopath_subset(clinDataInput())
        p_ = pl[[2]] ; n_col = ceiling(sqrt(length(p_)))
        if (n_col == 0) { grid.arrange(nullGrob()) }
        if (n_col != 0) {grid.arrange(grobs = p_, ncol=n_col) }
    }, height = 800, width = 900)


}


shinyApp(ui = ui, server = server)
      
