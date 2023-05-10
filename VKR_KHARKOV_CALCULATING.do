		** ВКР: Влияние инвестиций в ИКТ на экономический рост **

							** Харьков Роман **

								** ЭФ МГУ **
								
outreg using table1, summstat(N\ar1p\ar2p\sarganp\hansenp\j\kmo) ///
 summtitle(N\AR(1) -  p value \AR(2) - p value\ Sargan  -  p value \ Hansen - p value\ Instruments\kmo ) ///
 summdec(0 3 3 3 3 0 3) starlevels(10 5 1) starloc(1)

*Установка пакетов 
*ssc install xtabond2
*ssc install utest
*ssc install outreg
*ssc install mat2txt 
						
								
								** Исследование группы стран **							
								
use "/Users/romeo/Desktop/data1.dta", clear
egen ind = group(CODE) 
xtset ind t  /*придать панельную структуру данным*/

** трехлетние средние **

***Сгенерируем различные ICT показатели*** 
 
gen ln_ICT_INV=ln(ICT_INV) 
gen ln_LF=ln(LF) 
gen l_GDPPC=L1.GDPPC 
gen ICT=ln(ICT_INV_REAL)
gen ICT_DEV=ICT*DEVELOPE 
gen ICT2=ICT^2 
gen ICT2_DEV=ICT_DEV^2 
gen lnMU=ln(MU)
gen lnUI=ln(UI)

											*I*
						*Составляем модели c учетом временных эффектов* 
 
*1) Только ICT* 
 
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.INV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV, lag(. .)) ///
iv(i.t ) 
two robust
	*inf bad*
 
*2) Модификация ICT развивающихся и развитых* 
 
 xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.INV L1.ICT_DEV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV, lag(. .)) ///
iv(i.t ) 
 two robust
	*ICT_DEV bad* 

*3) Модификация ICT развивающихся и развитых и квадрата ICT* 
 
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.ICT_DEV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2, lag(. .)) ///
iv(i.t ) 
two robust
	**хорошие результаты**
 
 
*4) Модификация ICT и квадрата ICT* 
 
 xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2, lag(. .)) ///
iv(i.t ) 
 two robust 
	*ICT2 & INF_CP bad* 
 
*5) Модификация ICT развивающихся и развитых и квадрата развитых* 
 
 xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.INV L1.ICT_DEV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2_DEV, lag(. .)) ///
iv(i.t ) 
 two robust
	** хорошие результаты **
 
*6) Модификация ICT развивающихся и развитых и квадрата ICT* 
 
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.ICT2 L1.INV L1.ICT_DEV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2_DEV L2.ICT2, lag(. .)) ///
iv(i.t ) 
two robust
	*ICT & ICT2 bad*
 
*7) Модификация ICT и квадрата ICT развивающихся и развитых * 
 
 xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.ICT2 L1.INV i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2_DEV L2.ICT2, lag(. .)) ///
iv(i.t ) 
 two robust
	**хорошие результаты**
 
 
		**Смотрим, где получились хорошие результаты и переходим к следующей части**
		

										*II*
							*Добавляем переменные факторов ИКТ*	
		
*1) Модификация ICT развивающихся и развитых и квадрата ICT* 

	*MU*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.ICT_DEV L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2 L2.lnMU, lag(. .)) ///
iv(i.t ) 
two robust

		*ICT & ICT2 ICT_DEV bad*

	*UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.ICT_DEV L1.lnUI i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2 L2.lnUI, lag(. .)) ///
iv(i.t ) 
two robust

		*good results*
	
	*MU & UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.ICT_DEV L1.lnUI L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2 L2.lnUI L2.lnMU, lag(. .)) ///
iv(i.t ) 
two robust

		*ICT & ICT2 & lnMU & ICT_DEV bad*

*2) Модификация ICT развивающихся и развитых и квадрата развитых* 

	*MU*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.INV L1.ICT_DEV L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2_DEV L2.lnMU, lag(. .)) ///
iv(i.t ) 
two robust
		** хорошие результаты **
		
	*UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.INV L1.ICT_DEV L1.lnUI i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2_DEV L2.lnUI, lag(. .)) ///
iv(i.t ) 
two robust
		** хорошие результаты **
		
	*MU & UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.INV L1.ICT_DEV L1.lnUI L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT_DEV L2.ICT2_DEV L2.lnUI L2.lnMU, lag(. .)) ///
iv(i.t ) 
two robust
		**MU bad**
		
*3) Модификация ICT и квадрата ICT развивающихся и развитых * 

	*MU*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.ICT2 L1.INV L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2_DEV L2.ICT2 L2.lnMU, lag(. .)) ///
iv(i.t ) 
 two robust
		*ICT & ICT2 & ICT2_DEV bad*
 
	*UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.ICT2 L1.INV L1.lnUI i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2_DEV L2.ICT2 L2.lnUI, lag(. .)) ///
iv(i.t ) 
 two robust
		*good results*
 
	*MU & UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2_DEV L1.ICT2 L1.INV L1.lnUI L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2_DEV L2.ICT2 L2.lnUI L2.lnMU, lag(. .)) ///
iv(i.t ) 
 two robust
		*ICT & ICT2 & ICT2_DEV & MU bad*
 
					*Пробуем модификации с плохими моделями*
 
*4) Модификация ICT и квадрата ICT* 
 
	*UI*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.lnUI i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2 L2.lnUI, lag(. .)) ///
iv(i.t ) 
two robust
		*good results*

	*MU*
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.lnMU i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2 L2.lnMU, lag(. .)) ///
iv(i.t ) 
two robust 
		*so bad*
 
xtabond2 GDP_growth L1.GDP_growth L1.INF_CP L1.ICT L1.ICT2 L1.INV L1.lnMU L1.lnUI i.t , /// 
gmm( L2.GDP_growth L2.INF_CP L2.ICT L2.GOV L2.ICT2 L2.lnMU L2.lnUI, lag(. .)) ///
iv(i.t ) 
two robust 
		*ICT & ICT2 & MU bad*
		
		
		
*__________________________________________________________________________________________*

			** Настало время посмотреть модели для регионов России (с другими данными) **

*__________________________________________________________________________________________*
			
		** для начала посмотрим для трехлетних средних -> не вышло, мало джанных **
			
** теперь посмотрим для двухлетних средних **
			
use "/Users/romeo/Desktop/data3.dta", clear
egen ind = group(REGION) 
xtset ind t

****Сгенерируем различные показатели*****

gen lnK=ln(K)
gen lnL=ln(L)
gen lnMU=ln(MU)
gen lnICT=ln(ICT)
gen lnICT2=lnICT^2
gen l_lnICT2=L1.lnICT2
gen diff_lnICT2=lnICT2-l_lnICT2
gen l_lnICT=L1.lnICT
gen diff_lnICT=lnICT-l_lnICT
gen l_lnK=L1.lnK
gen diff_lnK=lnK-l_lnK
gen l_lnL=L1.lnL
gen diff_lnL=lnL-l_lnL
gen l_lnMU=L1.lnMU
gen diff_lnMU=lnMU-l_lnMU


											*I*
						*Составляем модели c учетом временных эффектов* 

	*модель общего вида для модификаций*
xtabond2 GRP_IND diff_lnL diff_lnK diff_lnICT diff_lnMU diff_lnICT2 i.t, ///
gmm(L1.GRP_IND L1.diff_lnL L1.diff_lnK L1.diff_lnICT L1.diff_lnMU L1.diff_lnICT2, lag(. .)) ///
iv(i.t )
two robust 
*gooooood results bad ICT*
						
* 1) Модель с MU & ICT2 *
xtabond2 GRP_IND diff_lnL diff_lnK diff_lnMU diff_lnICT2 i.t, ///
gmm(L1.GRP_IND L1.diff_lnL L1.diff_lnK L1.diff_lnMU L1.diff_lnICT2, lag(. .)) ///
iv(i.t )
two robust 
*gooooood results*


* 2) Модель с ICT *
xtabond2 GRP_IND diff_lnL diff_lnK diff_lnICT  i.t, ///
gmm(L1.GRP_IND L1.diff_lnL L1.diff_lnK L1.diff_lnICT, lag(. .)) ///
iv(i.t )
two robust 
*gooooood results*

* 3) Модель с IC2 *
xtabond2 GRP_IND diff_lnL diff_lnK diff_lnICT2  i.t, ///
gmm(L1.GRP_IND L1.diff_lnL L1.diff_lnK L1.diff_lnICT2, lag(. .)) ///
iv(i.t )
two robust 
*gooooood results*

* 4) Модель с MU *
xtabond2 GRP_IND diff_lnL diff_lnK diff_lnMU  i.t, ///
gmm(L1.GRP_IND L1.diff_lnL L1.diff_lnK L1.diff_lnMU, lag(. .)) ///
iv(i.t )
two robust 
*gooooood results*

outreg using table17, summstat(N\ar1p\ar2p\sarganp\hansenp\j\kmo) ///
 summtitle(N\AR(1) -  p value \AR(2) - p value\ Sargan  -  p value \ Hansen - p value\ Instruments\kmo ) ///
 summdec(0 3 3 3 3 0 3) starlevels(10 5 1) starloc(1)
