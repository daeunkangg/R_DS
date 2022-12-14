#1. csv file $)C:R7/?@1b
working = read.csv('working.csv', fileEncoding='euc-kr')

#2. $)C5%@LEM FD>GGO1b
head(working)
str(working)
dim(working)

summary(working)
table(working$$)CAv?*)
table(working$$)C>wA>)
table(working$$)C;g>wC<1T8p)
table(working$$)C?,7I4k)
table(working$$)C<::0)
table(working$$)CGP7B)
table(working$$)CAw>w)
table(working$$)C?yFr1U<R5f180#)

#3. $)C5%@LEM A$A&
#$)C0aCxD! ?):N H.@N
table(is.na(working))
table(is.na(working$$)CAv?*))
table(is.na(working$$)C>wA>))
table(is.na(working$$)C;g>wC<1T8p))
table(is.na(working$$)C?,BwH^0!:N?)))
table(is.na(working$$)C?,BwH^0!;g?k))
table(is.na(working$$)C?,7I4k))
table(is.na(working$$)C<::0))
table(is.na(working$$)CGP7B))
table(is.na(working$$)CAw>w))
table(is.na(working$$)C?yFr1U<R5f180#))
#$)CH.@N 0a0z ?,BwH^0!:N?), ?,BwH^0!;g?k ?-?!<- 0aCx0*@L 0"0" 5,903 A8@gGT H.@N

#$)C0aCxD! A&0E
working_narm = working[!is.na(working$$)C?,BwH^0!:N?)), ]
dim(working_narm)
working_new = working_narm[!is.na(working_narm$$)C?,BwH^0!;g?k), ]
dim(working_new)
#$)CCQ 1003@G G`@L A&0E 5G>z@=; @L8& 9L7g>n :8>F 0aCxD!0! A_:95G4B G`?! @V>z@=@; A|@[GR <v @V@=.


#4. $)C5%@LEM :P<.
#4-1. $)C9|AV :/<v:0 ?,BwH^0!:N?), ?,BwH^0!;g?k Fr1U :q13

#$)CAv?*
working_new %>%
  select($)CAv?*,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)CAv?*) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#17$)C03 8p5g Av?*?!<- :N?)5H ?,Bw?! :qGX ;g?k5H ?,Bw0! @{@=. 4kC<7N 4k55=C?!<- ?,Bw :N?)?M ;g?k@L 89@=.

#$)C>wA>
working_new %>%
  select($)C>wA>,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C>wA>) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#$)C@|1b, 0!=:, Au1b 9W <v55;g>w A>;g@Z5i@G ?,Bw :N?)?M ;g?k@L 0!@e 89>R@88g, 0G<3>w A>;g@Z@G ?,Bw :N?)?M ;g?k@L 0!@e @{>z@=.

#$)C;g>wC<1T8p
working_new %>%
  select($)C;g>wC<1T8p,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C;g>wC<1T8p) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k),vac_ratio = used_vac/given_vac*100)
#$)CA>;g@Z 1T8p0! E,<v7O Fr1U@{@87N :N?)GQ ?,Bw @O<v0! Au0!GT.

#$)C<::0
working_new %>%
  select($)C<::0,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C<::0) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#$)CFr1U@{@87N 32<:?!0T 4u 89@: ?,Bw0! :N?)5G>z@88g, 32<:@L 4u 89@: ?,Bw8& ;g?kG_4Y.

#$)CGP7B
working_new %>%
  select($)CGP7B,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)CGP7B) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#$)CGP7B@L 3t@;<v7O ?,Bw:N?)?M ;g?k@L H.?,GO0T Au0!GT@; H.@NGR <v @V@=.

#$)CAw>w
working_new %>%
  select($)CAw>w,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)CAw>w) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#$)C@|9.Aw 9W 0|8.Aw?!<- ?,Bw ;g?k@L 89>R@88g, FG8E A>;g@Z 9W 4\<x3k9+ A>;g@Z@G ?,Bw ;g?k :q@2@L 37>R@=

#$)C?,7I4k
working_new %>%
  select($)C?,7I4k,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C?,7I4k) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#30$)C4k0! 0!@e 89@: ?,BwH^0!:N?)?M ?,BwH^0!;g?k@; 1b7OG_Av88, ;g?k5H :q@2@: 1W780T @/@G9LGO0T 3tAv >J@=.

#$)C?yFr1U<R5f180#
working_new %>%
  select($)C?yFr1U<R5f180#,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C?yFr1U<R5f180#) %>%
  summarise(given_vac = mean($)C?,BwH^0!:N?)), used_vac = mean(?,BwH^0!;g?k), vac_ratio = used_vac/given_vac*100)
#$)C?yFr1U<R5f180#@L 3t@=?! 5{6s ?,Bw:N?)?M ;g?k@L 89@=

#4-2. 2030$)C<<4k?M 5060<<4k@G GP7B:0 :PFw

#2030$)C4k GP7B :PFw H.@N
working2030 <- working_new %>%
  filter($)C?,7I4k=='204k'|?,7I4k=='304k') %>%
  group_by($)CGP7B) %>%
  summarise(number=n(),ratio=number/3036*100)
working2030

#$)C874k 1W7!GA7N =C0"H-
ggplot(working2030, aes(x = $)CGP7B, y = number)) +
  xlab('education') +
  geom_col()
#$)CH.@N 0a0z 4kA9@L 0!@e 89>R@88g, 1W 4Y@=@87N @|9.4kA9, 0mA9, 4kGP?x, A_A9@LGO <x
  
#5060$)C4k GP7B :;Fw H.@N
working5060 <- working_new %>%
  filter($)C?,7I4k=='504k'|?,7I4k=='604k') %>%
  group_by($)CGP7B) %>%
  summarise(number=n(),ratio=number/617*100)
working5060

#$)C874k1W7!GA7N =C0"H-
ggplot(working5060, aes(x = $)CGP7B, y = number)) +
  xlab('education') +
  geom_col()
#$)CH.@N 0a0z 4kA90z 0mA9@G :q@2@L :q=AGT.

#4-3. $)C;g>wC<1T8p?! 5{8% <::0 ?yFr1U<R5f180# Fr1U :q13

#$)C;g>wC<1T8p 20@N 9L88 H8;g@G <::0 ?yFr1U<R5f180#
working_new %>%
  filter($)C;g>wC<1T8p>=1&;g>wC<1T8p<=3) %>%
  select($)C<::0,?yFr1U<R5f180#) %>%
  group_by($)C<::0) %>%
  summarise(earning_mean=mean($)C?yFr1U<R5f180#))
#$)C32<:@G Fr1U@{@N ?yFr1U<R5f180#@L ?)<:@G Fr1U@{@N ?yFr1U<R5f180#:84Y 3t@=.
  
#$)C;g>wC<1T8p 20@N @L;s H8;g@G <::0 ?yFr1U<R5f180#
working_new %>%
  filter($)C;g>wC<1T8p>=4&;g>wC<1T8p<=6) %>%
  select($)C<::0,?yFr1U<R5f180#) %>%
  group_by($)C<::0) %>%
  summarise(earning_mean=mean($)C?yFr1U<R5f180#))
#$)C>U<1 0a0z?M :q=AGOAv88, ?)<:0z 32<: 0#@G Bw@L0! @{@=

#$)C<::0 9+0|GQ ;g>wC< 1T8p :q13
working_new %>%
  select($)C;g>wC<1T8p,?yFr1U<R5f180#) %>%
  group_by($)C;g>wC<1T8p) %>%
  summarise(earning_mean=mean($)C?yFr1U<R5f180#))

#$)C;g>wC< 1T8p?M 9+0|GQ <::0 :q13
working_new %>%
  select($)C<::0,?yFr1U<R5f180#) %>%
  group_by($)C<::0) %>%
  summarise(earning_mean=mean($)C?yFr1U<R5f180#))

#$)CA&AV55 C_0! :P<.
working_new %>%
  filter($)CAv?*==17) %>%
  select($)C>wA>,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C>wA>) %>%
  summarise(given_vac=mean($)C?,BwH^0!:N?)), used_vac=mean(?,BwH^0!;g?k), n=n())
#$)C?n<v>w, <w9Z @==DA! 5n@G ?5<<>w@Z 4Y<v
working_new %>%
  filter($)CAv?*==17) %>%
  select($)C;g>wC<1T8p,?,BwH^0!:N?),?,BwH^0!;g?k) %>%
  group_by($)C;g>wC<1T8p) %>%
  summarise(given_vac=mean($)C?,BwH^0!:N?)), used_vac=mean(?,BwH^0!;g?k), n=n())
# $)C;g>wC<1T8p0! @[@: 1b>w 9PA}