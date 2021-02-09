# word-highlight-mode.el

## TODO
- [ ] face指定がうまくいってない。deffaceで定義したものを使いたい。add-text-propertyがよくない感じか？
- [/] faceを消すとき、undoの履歴に残ってしまう -> overlayを使うことによりクリア
- [/] なんか遅い
- [ ] 括弧にカーソルが当たったときにsexpを選択するようになっていない。(easy-mark-word)と揃えたい
- [ ] もっといいword指定のやり方はないのか？最初のforward-worddなどあまりよくない気がする…これが遅さの原因かも。
- [/] 初回の実行が失敗する(最初にdelete-overlayするが、最初はないので)

- ahs-modeはcurrent-wordだけ色が違うよう。これは参考にできそう。全体検索して色を変えればいい？
