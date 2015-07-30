Gomoku
============================

## バージョン

- 0.0.7

## 概要

- Igo( http://igo.sourceforge.jp/ )から派生した形態素解析器
- 辞書データがあらかじめjarファイルに含まれていることが特徴
- デフォルトではIPADIC(mecab-ipadic-2.7.0-20070801)を使用している
  - common lispで書かれた辞書構築コマンドを使うことでカスタマイズは可能
  - 現状ではIPADIC以外に対応しようとする場合は(おそらく)ソースコードの修正が必要
- 素性としては品詞情報のみを保持
  - 原型や読みの情報などの情報は破棄している
- その他、形態素解析器としての特徴は、おおむねIgoと同様


## 形態素解析器

形態素解析を行う(Java)。

### ビルド方法

```bash
cd analyzer
ant
ls gomoku-x.x.x.jar
```

### 形態素解析コマンド

```bash
# 形態素解析
java -cp gomoku-x.x.x.jar net.reduls.gomoku.bin.Gomoku < 解析対象テキスト

# 分かち書き
java -cp gomoku-x.x.x.jar net.reduls.gomoku.bin.Gomoku -wakati < 解析対象テキスト
```

### java API

```java
package net.reduls.gomoku;

class Tagger {
  public static List<Morpheme> parse(String text);
  public static List<String> wakati(String text);
}

class Morpheme {
  public final String surface;  // 形態素表層形
  public final String feature;  // 形態素素性 (== 品詞)
  public final int start;       // 入力テキスト内での形態素の出現開始位置
}
```

## 辞書構築コマンド

Gomoku用のバイナリ辞書を構築する(Common Lisp)。
デフォルトの辞書(IPADIC)をカスタマイズしたい場合は、このコマンドを使ってバイナリ辞書を作成し、jarを再ビルドする必要がある。

ソース辞書にはMecabのサイト( http://mecab.sourceforge.net/ )にて配布されている辞書を想定。

※ 現時点ではIPADICにのみ対応。他の辞書での動作は未確認。


### 依存パッケージ

- SBCL: http://www.sbcl.org/
  - Common Lisp処理系

### 辞書構築コマンド作成方法

```bash
cd dicbuilder
sbcl --script make-build-dic-command.lisp [コマンドの作成先ディレクトリ]
ls gomoku-build-dic
```

### 辞書構築コマンド使用方法

```bash
gomoku-build-dic <入力テキスト辞書ディレクトリ> <出力バイナリ辞書ディレクトリ> [テキスト辞書の文字コード(デフォルトはeuc-jp)]
# 注意! ディレクトリ指定は最後の'/'が必要
```

### Gomoku辞書の更新手順

```bash
cd analyzer
gomoku-build-dic mecab-ipadic-2.7.0-20070801/ src/net/reduls/gomoku/dicdata/  # 新しい辞書データで上書き
ant clean
ant
```

## ライセンス

- Gomoku本体(ソースファイル及びjarファイル)
  - MITライセンス: 詳細は[COPYING](COPYING)ファイルを参照
- Gomokuに含まれるIPADICバイナリデータ
　- IPADICのライセンスに準拠: 詳細はCOPYING.ipadicを参照


## TODO

- サロゲートペア対応
- CSVパーサ
- IPADIC以外の辞書に対応
- ヒープ実装最適化
- 諸々整理
