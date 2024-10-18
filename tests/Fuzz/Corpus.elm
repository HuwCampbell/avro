module Fuzz.Corpus exposing (boats, glass, waters, weather)

{-| This modules contains definitions from the Hedgehog Corpus package.
-}

import Fuzz exposing (Fuzzer)


waters : Fuzzer String
waters =
    Fuzz.oneOfValues
        [ "basin"
        , "bay"
        , "billabong"
        , "canal"
        , "channel"
        , "creek"
        , "estuary"
        , "fjord"
        , "harbour"
        , "lake"
        , "loch"
        , "marsh"
        , "ocean"
        , "pond"
        , "puddle"
        , "reservoir"
        , "river"
        , "sea"
        , "slough"
        , "sound"
        , "spring"
        , "stream"
        , "swamp"
        , "wetland"
        ]


boats : Fuzzer String
boats =
    Fuzz.oneOfValues
        [ "barge"
        , "battleship"
        , "canoe"
        , "catamaran"
        , "dinghy"
        , "ferry"
        , "gondola"
        , "jetski"
        , "kayak"
        , "longship"
        , "motorboat"
        , "pontoon"
        , "powerboat"
        , "rowboat"
        , "ship"
        , "steamboat"
        , "tanker"
        , "trawler"
        , "tugboat"
        , "yacht"
        ]


weather : Fuzzer String
weather =
    Fuzz.oneOfValues
        [ "dry"
        , "raining"
        , "hot"
        , "humid"
        , "snowing"
        , "fresh"
        , "windy"
        , "freezing"
        ]



-- | How to say "I can eat glass, and it doesn't hurt me." in a few different
--   languages.
--
--   From: http://kermitproject.org/utf8.html
--


glass : Fuzzer String
glass =
    Fuzz.oneOfValues
        [ "काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥" -- Sanskrit
        , "kācaṃ śaknomyattum; nopahinasti mām." -- Sanskrit (standard transcription)
        , "ὕαλον ϕαγεῖν δύναμαι· τοῦτο οὔ με βλάπτει." -- Classical Greek
        , "Μπορώ να φάω σπασμένα γυαλιά χωρίς να πάθω τίποτα." -- Greek (monotonic)
        , "Μπορῶ νὰ φάω σπασμένα γυαλιὰ χωρὶς νὰ πάθω τίποτα. " -- Greek (polytonic)
        , "Vitrum edere possum; mihi non nocet." -- Latin
        , "Je puis mangier del voirre. Ne me nuit." -- Old French
        , "Je peux manger du verre, ça ne me fait pas mal." -- French
        , "Pòdi manjar de veire, me nafrariá pas." -- Provençal / Occitan
        , "J'peux manger d'la vitre, ça m'fa pas mal." -- Québécois
        , "Dji pou magnî do vêre, çoula m' freut nén må. " -- Walloon
        , "Ch'peux mingi du verre, cha m'foé mie n'ma. " -- Picard
        , "Mwen kap manje vè, li pa blese'm." -- Kreyòl Ayisyen (Haitï)
        , "Kristala jan dezaket, ez dit minik ematen." -- Basque
        , "Puc menjar vidre, que no em fa mal." -- Catalan / Català
        , "Puedo comer vidrio, no me hace daño." -- Spanish
        , "Puedo minchar beire, no me'n fa mal . " -- Aragonés
        , "Eu podo xantar cristais e non cortarme." -- Galician
        , "Posso comer vidro, não me faz mal." -- European Portuguese
        , "Posso comer vidro, não me machuca." -- Brazilian Portuguese
        , "M' podê cumê vidru, ca ta maguâ-m '." --Caboverdiano/Kabuverdianu (Cape Verde)
        , "Ami por kome glas anto e no ta hasimi daño." -- Papiamentu
        , "Posso mangiare il vetro e non mi fa male." -- Italian
        , "Sôn bôn de magnà el véder, el me fa minga mal." -- Milanese
        , "Me posso magna' er vetro, e nun me fa male." -- Roman
        , "M' pozz magna' o'vetr, e nun m' fa mal." -- Napoletano
        , "Mi posso magnare el vetro, no'l me fa mae." -- Venetian
        , "Pòsso mangiâ o veddro e o no me fà mâ." -- Zeneise (Genovese)
        , "Puotsu mangiari u vitru, nun mi fa mali. " -- Sicilian
        , "Jau sai mangiar vaider, senza che quai fa donn a mai. " -- Romansch (Grischun)
        , "Pot să mănânc sticlă și ea nu mă rănește." -- Romanian
        , "Mi povas manĝi vitron, ĝi ne damaĝas min. " -- Esperanto
        , "Mý a yl dybry gwéder hag éf ny wra ow ankenya." -- Cornish
        , "Dw i'n gallu bwyta gwydr, 'dyw e ddim yn gwneud dolur i mi." -- Welsh
        , "Foddym gee glonney agh cha jean eh gortaghey mee." -- Manx Gaelic
        , "᚛᚛ᚉᚑᚅᚔᚉᚉᚔᚋ\u{1680}ᚔᚈᚔ\u{1680}ᚍᚂᚐᚅᚑ\u{1680}ᚅᚔᚋᚌᚓᚅᚐ᚜" -- Old Irish (Ogham)
        , "Con·iccim ithi nglano. Ním·géna." -- Old Irish (Latin)
        , "Is féidir liom gloinne a ithe. Ní dhéanann sí dochar ar bith dom." -- Irish
        , "Ithim-s a gloine agus ní miste damh é." --Ulster Gaelic
        , "S urrainn dhomh gloinne ithe; cha ghoirtich i mi." -- Scottish Gaelic
        , "ᛁᚳ᛫ᛗᚨᚷ᛫ᚷᛚᚨᛋ᛫ᛖᚩᛏᚪᚾ᛫ᚩᚾᛞ᛫ᚻᛁᛏ᛫ᚾᛖ᛫ᚻᛖᚪᚱᛗᛁᚪᚧ᛫ᛗᛖ᛬" -- Anglo-Saxon (Runes)
        , "Ic mæg glæs eotan ond hit ne hearmiað me." -- Anglo-Saxon (Latin)
        , "Ich canne glas eten and hit hirtiþ me nouȝt." -- Middle English
        , "I can eat glass and it doesn't hurt me." -- English
        , "[aɪ kæn iːt glɑːs ænd ɪt dɐz nɒt hɜːt miː]" -- English (IPA)
        , "⠊⠀⠉⠁⠝⠀⠑⠁⠞⠀⠛⠇⠁⠎⠎⠀⠁⠝⠙⠀⠊⠞⠀⠙⠕⠑⠎⠝⠞⠀⠓⠥⠗⠞⠀⠍⠑" -- English (Braille)
        , "Mi kian niam glas han i neba hot mi." -- Jamaican
        , "Ah can eat gless, it disnae hurt us. " -- Lalland Scots / Doric
        , "𐌼𐌰𐌲 𐌲𐌻𐌴𐍃 𐌹̈𐍄𐌰𐌽, 𐌽𐌹 𐌼𐌹𐍃 𐍅𐌿 𐌽𐌳𐌰𐌽 𐌱𐍂𐌹𐌲𐌲𐌹𐌸." -- Gothic
        , "ᛖᚴ ᚷᛖᛏ ᛖᛏᛁ ᚧ ᚷᛚᛖᚱ ᛘᚾ ᚦᛖᛋᛋ ᚨᚧ ᚡᛖ ᚱᚧᚨ ᛋᚨᚱ" -- Old Norse (Runes)
        , "Ek get etið gler án þess að verða sár." -- Old Norse (Latin)
        , "Eg kan eta glas utan å skada meg." -- Norsk / Norwegian (Nynorsk)
        , "Jeg kan spise glass uten å skade meg." -- Norsk / Norwegian (Bokmål)
        , "Eg kann eta glas, skaðaleysur." -- Føroyskt / Faroese
        , "Ég get etið gler án þess að meiða mig." -- Íslenska / Icelandic
        , "Jag kan äta glas utan att skada mig." -- Svenska / Swedish
        , "Jeg kan spise glas, det gør ikke ondt på mig." -- Dansk / Danish
        , "Æ ka æe glass uhen at det go mæ naue." -- Sønderjysk
        , "Ik kin glês ite, it docht me net sear." -- Frysk / Frisian
        , "Ik kan glas eten, het doet mĳ geen kwaad." -- Nederlands / Dutch
        , "Iech ken glaas èèse, mer 't deet miech jing pieng." -- Kirchröadsj/Bôchesserplat
        , "Ek kan glas eet, maar dit doen my nie skade nie." -- Afrikaans
        , "Ech kan Glas iessen, daat deet mir nët wei." -- Lëtzebuergescht / Luxemburgish
        , "Ich kann Glas essen, ohne mir zu schaden." -- Deutsch / German
        , "Ich kann Glas verkasematuckeln, ohne dattet mich wat jucken tut." -- Ruhrdeutsch
        , "Isch kann Jlaas kimmeln, uuhne datt mich datt weh dääd." -- Langenfelder Platt
        , "Ich koann Gloos assn und doas dudd merr ni wii." -- Lausitzer Mundart ("Lusatian")
        , "Iech konn glaasch voschbachteln ohne dass es mir ebbs daun doun dud." -- Odenwälderisch
        , "'sch kann Glos essn, ohne dass'sch mer wehtue." -- Sächsisch / Saxon
        , "Isch konn Glass fresse ohne dasses mer ebbes ausmache dud." -- Pfälzisch
        , "I kå Glas frässa, ond des macht mr nix!" -- Schwäbisch / Swabian
        , "I ka glas eassa, ohne dass mar weh tuat." -- Deutsch (Voralberg)
        , "I koh Glos esa, und es duard ma ned wei." -- Bayrisch / Bavarian
        , "I kaun Gloos essen, es tuat ma ned weh." -- Allemannisch
        , "Ich chan Glaas ässe, das schadt mir nöd." -- Schwyzerdütsch (Zürich)
        , "Ech cha Glâs ässe, das schadt mer ned. " -- Schwyzerdütsch (Luzern)
        , "Meg tudom enni az üveget, nem lesz tőle bajom." -- Hungarian
        , "Voin syödä lasia, se ei vahingoita minua." -- Suomi / Finnish
        , "Sáhtán borrat lása, dat ii leat bávččas." -- Sami (Northern)
        , "Мон ярсан суликадо, ды зыян эйстэнзэ а ули." -- Erzian
        , "Mie voin syvvä lasie ta minla ei ole kipie." -- Northern Karelian
        , "Minä voin syvvä st'oklua dai minule ei ole kibie. " -- Southern Karelian
        , "Ma võin klaasi süüa, see ei tee mulle midagi." -- Estonian
        , "Es varu ēst stiklu, tas man nekaitē." -- Latvian
        , "Aš galiu valgyti stiklą ir jis manęs nežeidžia " -- Lithuanian
        , "Mohu jíst sklo, neublíží mi." -- Czech
        , "Môžem jesť sklo. Nezraní ma." -- Slovak
        , "Mogę jeść szkło i mi nie szkodzi." -- Polska / Polish
        , "Lahko jem steklo, ne da bi mi škodovalo." -- Slovenian
        , "Ja mogu jesti staklo, i to mi ne šteti." -- Bosnian, Croatian, Montenegrin and Serbian (Latin)
        , "Ја могу јести стакло, и то ми не штети." -- Bosnian, Montenegrin and Serbian (Cyrillic)
        , "Можам да јадам стакло, а не ме штета." -- Macedonian
        , "Я могу есть стекло, оно мне не вредит." -- Russian
        , "Я магу есці шкло, яно мне не шкодзіць." -- Belarusian (Cyrillic)
        , "Ja mahu jeści škło, jano mne ne škodzić." -- Belarusian (Lacinka)
        , "Я можу їсти скло, і воно мені не зашкодить." -- Ukrainian
        , "Мога да ям стъкло, то не ми вреди." -- Bulgarian
        , "მინას ვჭამ და არა მტკივა." -- Georgian
        , "Կրնամ ապակի ուտել և ինծի անհանգիստ չըներ։" -- Armenian
        , "Unë mund të ha qelq dhe nuk më gjen gjë." -- Albanian
        , "Cam yiyebilirim, bana zararı dokunmaz." -- Turkish
        , "جام ييه بلورم بڭا ضررى طوقونمز" -- Turkish (Ottoman)
        , "Men shisha yeyishim mumkin, ammo u menga zarar keltirmaydi." -- Uzbek / O’zbekcha (Roman)
        , "Мен шиша ейишим мумкин, аммо у менга зарар келтирмайди." -- Uzbek / Ўзбекча (Cyrillic)
        , "আমি কাঁচ খেতে পারি, তাতে আমার কোনো ক্ষতি হয় না।" -- Bangla / Bengali
        , "मी काच खाऊ शकतो, मला ते दुखत नाही." -- Marathi
        , "ನನಗೆ ಹಾನಿ ಆಗದೆ, ನಾನು ಗಜನ್ನು ತಿನಬಹುದು" -- Kannada
        , "मैं काँच खा सकता हूँ और मुझे उससे कोई चोट नहीं पहुंचती." -- Hindi
        , "എനിക്ക് ഗ്ലാസ് തിന്നാം. അതെന്നെ വേദനിപ്പിക്കില്ല." -- Malayam
        , "நான் கண்ணாடி சாப்பிடுவேன், அதனால் எனக்கு ஒரு கேடும் வராது." -- Tamil
        , "నేను గాజు తినగలను మరియు అలా చేసినా నాకు ఏమి ఇబ్బంది లేదు" -- Telugu
        , "මට වීදුරු කෑමට හැකියි. එයින් මට කිසි හානියක් සිදු නොවේ." -- Sinhalese
        , "میں کانچ کھا سکتا ہوں اور مجھے تکلیف نہیں ہوتی ۔" -- Urdu
        , "زه شيشه خوړلې شم، هغه ما نه خوږوي" -- Pashto
        , ".من می توانم بدونِ احساس درد شيشه بخورم" -- Farsi / Persian
        , "أنا قادر على أكل الزجاج و هذا لا يؤلمني. " -- Arabic
        , "Nista' niekol il-ħ ġieġ u ma jagħmilli xejn." --Maltese
        , "אני יכול לאכול זכוכית וזה לא מזיק לי." -- Hebrew
        , "איך קען עסן גלאָז און עס טוט מיר נישט װײ. " -- Yiddish
        , "Metumi awe tumpan, ɜnyɜ me hwee." -- Twi
        , "Inā iya taunar gilāshi kuma in gamā lāfiyā." -- Hausa (Latin)
        , "إِنا إِىَ تَونَر غِلَاشِ كُمَ إِن غَمَا لَافِىَا" -- Hausa (Ajami)
        , "Mo lè je̩ dígí, kò ní pa mí lára." -- Yoruba
        , "Nakokí kolíya biténi bya milungi, ekosála ngáí mabé tɛ́." -- Lingala
        , "Naweza kula bilauri na sikunyui." -- (Ki)Swahili
        , "Saya boleh makan kaca dan ia tidak mencederakan saya." -- Malay
        , "Kaya kong kumain nang bubog at hindi ako masaktan." -- Tagalog
        , "Siña yo' chumocho krestat, ti ha na'lalamen yo'." -- Chamorro
        , "Au rawa ni kana iloilo, ia au sega ni vakacacani kina." -- Fijian
        , "Aku isa mangan beling tanpa lara." -- Javanese
        , "က္ယ္ဝန္တော္၊က္ယ္ဝန္မ မ္ယက္စားနုိင္သည္။ ၎က္ရောင္ ထိခုိက္မ္ဟု မရ္ဟိပာ။" -- Burmese (Unicode 4.0)
        , "ကျွန်တော် ကျွန်မ မှန်စားနိုင်တယ်။ ၎င်းကြောင့် ထိခိုက်မှုမရှိပါ။" -- Burmese (Unicode 5.0)
        , "Tôi có thể ăn thủy tinh mà không hại gì." -- Vietnamese (quốc ngữ)
        , "些 𣎏 世 咹 水 晶 𦓡 空 𣎏 害 咦" -- Vietnamese (nôm)
        , "ខ្ញុំអាចញុំកញ្ចក់បាន ដោយគ្មានបញ្ហារ" -- Khmer
        , "ຂອ້ຍກິນແກ້ວໄດ້ໂດຍທີ່ມັນບໍ່ໄດ້ເຮັດໃຫ້ຂອ້ຍເຈັບ." -- Lao
        , "ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ" -- Thai
        , "Би шил идэй чадна, надад хортой биш" -- Mongolian (Cyrillic)
        , "ᠪᠢ ᠰᠢᠯᠢ ᠢᠳᠡᠶᠦ ᠴᠢᠳᠠᠨᠠ ᠂ ᠨᠠᠳᠤᠷ ᠬᠣᠤᠷᠠᠳᠠᠢ ᠪᠢᠰᠢ " -- Mongolian (Classic)
        , "म काँच खान सक्छू र मलाई केहि नी हुन्न् ।" -- Nepali
        , "ཤེལ་སྒོ་ཟ་ནས་ང་ན་གི་མ་རེད།" -- Tibetan
        , "我能吞下玻璃而不伤身体。" -- Chinese
        , "我能吞下玻璃而不傷身體。" -- Chinese (Traditional)
        , "Góa ē-t àng chia̍h po-lê, mā bē tio̍h-siong." -- Taiwanese
        , "私はガラスを食べられます。それは私を傷つけません。" -- Japanese
        , "나는 유리를 먹을 수 있어요. 그래도 아프지 않아요" -- Korean
        , "Mi save kakae glas, hemi no save katem mi." -- Bislama
        , "Hiki iaʻu ke ʻai i ke aniani; ʻaʻole nō lā au e ʻeha." -- Hawaiian
        , "E koʻana e kai i te karahi, mea ʻā, ʻaʻe hauhau." -- Marquesan
        , "ᐊᓕᒍᖅ ᓂᕆᔭᕌᖓᒃᑯ ᓱᕋᙱᑦᑐᓐᓇᖅᑐᖓ" -- Inuktitut
        , "Naika məkmək kakshət labutay, pi weyk ukuk munk-s ik nay." --Chinook Jargon
        , "Tsésǫʼ yishą́ągo bííníshghah dóó doo shił neezgai da. " -- Navajo
        , "mi kakne le nu citka le blaci .iku'i le se go'i na xrani mi" -- Lojban
        , "Ljœr ye caudran créneþ ý jor cẃran." -- Nórdicg
        ]
