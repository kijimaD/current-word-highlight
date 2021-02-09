# word-highlight-mode.el

## TODO
- [x] face指定がうまくいってない。deffaceで定義したものを使いたい。add-text-propertyがよくない感じか？
- [x] faceを消すとき、undoの履歴に残ってしまう -> overlayを使うことによりクリア
- [x] なんか遅い
- [x] 初回の実行が失敗する(最初にdelete-overlayするが、最初はないので)
- [ ] 括弧にカーソルが当たったときにsexpを選択するようになっていない。(easy-mark-word)と揃えたい
- [ ] もっといいword指定のやり方はないのか？最初のforward-worddなどださい
- [ ] 日本語入力のときおかしい。重要な目的の一つなのに…
- [ ] region選択を上書きしてしまう。ahsではどうしてる…？
