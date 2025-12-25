fips = data.frame(
  pref = c(
    "北 海 道", "青 森 県", "岩 手 県", "宮 城 県", "秋 田 県", "山 形 県", 
    "福 島 県", "茨 城 県", "栃 木 県", "群 馬 県", "埼 玉 県", "千 葉 県", 
    "東 京 都", "神 奈 川 県", "新 潟 県", "富 山 県", "石 川 県", "福 井 県", 
    "山 梨 県", "長 野 県", "岐 阜 県", "静 岡 県", "愛 知 県", "三 重 県", 
    "滋 賀 県", "京 都 府", "大 阪 府", "兵 庫 県", "奈 良 県", "和 歌 山 県", 
    "鳥 取 県", "島 根 県", "岡 山 県", "広 島 県", "山 口 県", "徳 島 県", 
    "香 川 県", "愛 媛 県", "高 知 県", "福 岡 県", "佐 賀 県", "長 崎 県", 
    "熊 本 県", "大 分 県", "宮 崎 県", "鹿 児 島 県", "沖 縄 県"
  ),
  Pref = c(
    "Hokkaido", "Aomori-ken", "Iwate-ken", "Miyagi-ken", "Akita-ken", "Yamagata-ken", 
    "Fukushima-ken", "Ibaraki-ken", "Tochigi-ken", "Gumma-ken", "Saitama-ken", "Chiba-ken", 
    "Tokyo-to", "Kanagawa-ken", "Niigata-ken", "Toyama-ken", "Ishikawa-ken", "Fukui-ken", 
    "Yamanashi-ken", "Nagano-ken", "Gifu-ken", "Shizuoka-ken", "Aichi-ken", "Mie-ken", 
    "Shiga-ken", "Kyoto-fu", "Osaka-fu", "Hyogo-ken", "Nara-ken", "Wakayama-ken", 
    "Tottori-ken", "Shimane-ken", "Okayama-ken", "Hiroshima-ken", "Yamaguchi-ken", "Tokushima-ken", 
    "Kagawa-ken", "Ehime-ken", "Kochi-ken", "Fukuoka-ken", "Saga-ken", "Nagasaki-ken", 
    "Kumamoto-ken", "Oita-ken", "Miyazaki-ken", "Kagoshima-ken", "Okinawa-ken"
  ))
fips = fips %>% mutate(pref = gsub(' ', '', pref), 
                       pref = gsub('県|府', '', pref),
                       pref = gsub('東京都', '東京', pref),
                       Pref = gsub('-ken|-fu|-to', '', Pref)); head(fips)
save(fips, file = 'fips.RDa')
