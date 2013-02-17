######### MeSHTERM ########
test_that("MeSHTERM", 
{
	# cols - test
	expect_equivalent(cols(MeSHTERM), c("MESHID","MESHTERM","MESHCATEGORY"))

	# keytypes - test
	expect_equivalent(keytypes(MeSHTERM), c("MESHID","MESHTERM","MESHCATEGORY"))

	# keys - test
	expect_true(nrow(keys(MeSHTERM,keytype="MESHID")) > 1)
	expect_true(nrow(keys(MeSHTERM,keytype="MESHTERM")) > 1)
	expect_true(nrow(keys(MeSHTERM,keytype="MESHCATEGORY")) > 1)

	# select - test 1
	k1 <- head(keys(MeSHTERM,keytype="MESHID"))
	expect_true(
		nrow(
			select(MeSHTERM,
				keys = k1,
				cols = c("MESHID","MESHTERM","MESHCATEGORY"),
				keytype = "MESHID"
			)
		) >= nrow(k1)
	)

	# select - test 2
	k2 <- head(keys(MeSHTERM,keytype="MESHTERM"))
	expect_true(
		nrow(
			select(MeSHTERM,
				keys = k2,
				cols = c("MESHID","MESHTERM","MESHCATEGORY"),
				keytype = "MESHTERM"
			)
		) >= nrow(k2)
	)

	# select - test 3
	k3 <- head(keys(MeSHTERM,keytype="MESHCATEGORY"))
	expect_true(
		nrow(
			select(MeSHTERM,
				keys = k3,
				cols = c("MESHID","MESHTERM","MESHCATEGORY"),
				keytype = "MESHCATEGORY"
			)
		) >= nrow(k3)
	)
	
}
)



######### MeSHMAPCOUNTS ########
test_that("MeSHMAPCOUNTS", 
{
	# cols - test
	expect_equivalent(cols(MeSHMAPCOUNTS), c("MAPNAME","COUNT"))

	# keytypes - test
	expect_equivalent(keytypes(MeSHMAPCOUNTS), c("MAPNAME"))

	# keys - test
	expect_true(nrow(keys(MeSHMAPCOUNTS,keytype="MAPNAME")) > 1)

	# select - test 1
	k1 <- head(keys(MeSHMAPCOUNTS,keytype="MAPNAME"))
	expect_true(
		nrow(
			select(MeSHMAPCOUNTS,
				keys = k1,
				cols = c("MAPNAME","COUNT"),
				keytype = "MAPNAME"
			)
		) >= nrow(k1)
	)
	
}
)


######### MeSHSYNONYM ########
test_that("MeSHSYNONYM", 
{
	# cols - test
	expect_equivalent(cols(MeSHSYNONYM), c("MESHID","MESHSYNONYM"))

	# keytypes - test
	expect_equivalent(keytypes(MeSHSYNONYM), c("MESHID"))

	# keys - test
	expect_true(nrow(keys(MeSHSYNONYM,keytype="MESHID")) > 1)

	# select - test 1
	k1 <- head(keys(MeSHSYNONYM,keytype="MESHID"))
	expect_true(
		nrow(
			select(MeSHSYNONYM,
				keys = k1,
				cols = c("MESHID","MESHSYNONYM"),
				keytype = "MESHID"
			)
		) >= nrow(k1)
	)

}
)


######### MeSHQUALIFIER ########
test_that("MeSHQUALIFIER", 
{
	# cols - test
	expect_equivalent(cols(MeSHQUALIFIER), c("QUALIFIERID", "SUBHEADING", "MESHID"))

	# keytypes - test
	expect_equivalent(keytypes(MeSHQUALIFIER), c("QUALIFIERID", "MESHID"))

	# keys - test
	expect_true(nrow(keys(MeSHQUALIFIER,keytype="QUALIFIERID")) > 1)
	expect_true(nrow(keys(MeSHQUALIFIER,keytype="MESHID")) > 1)

	# select - test 1
	k1 <- head(keys(MeSHQUALIFIER,keytype="QUALIFIERID"))
	expect_true(
		nrow(
			select(MeSHQUALIFIER,
				keys = k1,
				cols = c("QUALIFIERID", "SUBHEADING", "MESHID"),
				keytype = "QUALIFIERID"
			)
		) >= nrow(k1)
	)

	# select - test 2
	k2 <- head(keys(MeSHQUALIFIER,keytype="MESHID"))
	expect_true(
		nrow(
			select(MeSHQUALIFIER,
				keys = k2,
				cols = c("QUALIFIERID", "SUBHEADING", "MESHID"),
				keytype = "MESHID"
			)
		) >= nrow(k2)
	)

}
)


######### AOR & PCR ########
ctg <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","V","Z")
for(i in 1:length(ctg)){

	##### AOR #####
	AOR_CALL <- paste0("MeSH",ctg[i],"AOR")
	Obj <- eval(parse(text=AOR_CALL))
	test_that(AOR_CALL, 
		{	
		# cols - test
		expect_equivalent(cols(Obj), c("ANCESTORMESHID", "OFFSPRINGMESHID"))

		# keytypes - test
		expect_equivalent(keytypes(Obj), c("ANCESTORMESHID", "OFFSPRINGMESHID"))

		# keys - test
		expect_true(nrow(keys(Obj,keytype="ANCESTORMESHID")) > 1)
		expect_true(nrow(keys(Obj,keytype="OFFSPRINGMESHID")) > 1)

		# objectname - test
		expect_true(Obj@objectname == AOR_CALL)

		# select - test 1
		k1 <- head(keys(Obj,keytype="ANCESTORMESHID"))
		expect_true(
			nrow(
				select(Obj,
					keys = k1,
					cols = c("ANCESTORMESHID", "OFFSPRINGMESHID"),
					keytype = "ANCESTORMESHID"
				)
			) >= nrow(k1)
		)

		# select - test 2
		k2 <- head(keys(Obj,keytype="OFFSPRINGMESHID"))
		expect_true(
			nrow(
				select(Obj,
					keys = k2,
					cols = c("ANCESTORMESHID", "OFFSPRINGMESHID"),
					keytype = "OFFSPRINGMESHID"
				)
			) >= nrow(k2)
		)
	}
)


	##### PCR #####
	PCR_CALL <- paste0("MeSH",ctg[i],"PCR")
	Obj <- eval(parse(text=PCR_CALL))
	test_that(PCR_CALL, 
		{	
		# cols - test
		expect_equivalent(cols(Obj), c("PARENTMESHID", "CHILDMESHID"))

		# keytypes - test
		expect_equivalent(keytypes(Obj), c("PARENTMESHID", "CHILDMESHID"))

		# keys - test
		expect_true(nrow(keys(Obj,keytype="PARENTMESHID")) > 1)
		expect_true(nrow(keys(Obj,keytype="CHILDMESHID")) > 1)

		# objectname - test
		expect_true(Obj@objectname == PCR_CALL)

		# select - test 1
		k1 <- head(keys(Obj,keytype="PARENTMESHID"))
		expect_true(
			nrow(
				select(Obj,
					keys = k1,
					cols = c("PARENTMESHID", "CHILDMESHID"),
					keytype = "PARENTMESHID"
				)
			) >= nrow(k1)
		)

		# select - test 2
		k2 <- head(keys(Obj,keytype="CHILDMESHID"))
		expect_true(
			nrow(
				select(Obj,
					keys = k2,
					cols = c("PARENTMESHID", "CHILDMESHID"),
					keytype = "CHILDMESHID"
				)
			) >= nrow(k2)
		)
	}
)
}

##### 1000 OR problem ######
test_that("1000 OR problem",
{
k <- keys(MeSHTERM, keytype="MESHID")
expect_true(nrow(select(MeSHTERM, keys=k[1,], cols=c("MESHID","MESHTERM","MESHCATEGORY"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHTERM, keys=k[1:600,], cols=c("MESHID","MESHTERM","MESHCATEGORY"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHTERM, keys=k[1:1200,], cols=c("MESHID","MESHTERM","MESHCATEGORY"),keytype="MESHID"))>=1)

k <- keys(MeSHQUALIFIER, keytype="MESHID")
expect_true(nrow(select(MeSHQUALIFIER, keys=k[1,], cols=c("QUALIFIERID","SUBHEADING","MESHID"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHQUALIFIER, keys=k[1:600,], cols=c("QUALIFIERID","SUBHEADING","MESHID"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHQUALIFIER, keys=k[1:1200,], cols=c("QUALIFIERID","SUBHEADING","MESHID"),keytype="MESHID"))>=1)

k <- keys(MeSHSYNONYM, keytype="MESHID")
expect_true(nrow(select(MeSHSYNONYM, keys=k[1,], cols=c("MESHID","MESHSYNONYM"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHSYNONYM, keys=k[1:600,], cols=c("MESHID","MESHSYNONYM"),keytype="MESHID"))>=1)
expect_true(nrow(select(MeSHSYNONYM, keys=k[1:1200,], cols=c("MESHID","MESHSYNONYM"),keytype="MESHID"))>=1)

k <- keys(MeSHAAOR, keytype="ANCESTORMESHID")
expect_true(nrow(select(MeSHAAOR, keys=k[1,], cols=c("ANCESTORMESHID","OFFSPRINGMESHID"),keytype="ANCESTORMESHID"))>=1)
expect_true(nrow(select(MeSHAAOR, keys=k[1:600,], cols=c("ANCESTORMESHID","OFFSPRINGMESHID"),keytype="ANCESTORMESHID"))>=1)
expect_true(nrow(select(MeSHAAOR, keys=k[1:1200,], cols=c("ANCESTORMESHID","OFFSPRINGMESHID"),keytype="ANCESTORMESHID"))>=1)

k <- keys(MeSHAPCR, keytype="PARENTMESHID")
expect_true(nrow(select(MeSHAPCR, keys=k[1,], cols=c("PARENTMESHID","CHILDMESHID"),keytype="PARENTMESHID"))>=1)
expect_true(nrow(select(MeSHAPCR, keys=k[1:600,], cols=c("PARENTMESHID","CHILDMESHID"),keytype="PARENTMESHID"))>=1)
expect_true(nrow(select(MeSHAPCR, keys=k[1:1200,], cols=c("PARENTMESHID","CHILDMESHID"),keytype="PARENTMESHID"))>=1)

})