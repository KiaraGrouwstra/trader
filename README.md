# Usage

- make `.yml` files from the `.example` files in `src/main/resources`
- `sbt run`

# Testing XChange

- `mvn '-DfailIfNoTests=false' '-DskipIntegrationTests=false' test`
- `mvn '-DfailIfNoTests=false' '-Dtest=org.knowm.xchange.examples.yobit.*' '-DskipIntegrationTests=false' test`

# TODO

Project scope:
- trading behavior:
	- scrape history, persist, ML
- fix exchanges:
	- Poloniex: gives `ExchangeError`s
	- Yobit: [no AccountService/TradeService](https://github.com/timmolter/XChange/tree/develop/xchange-yobit/src/main/java/org/knowm/xchange/yobit/service), though endpoints [available](https://yobit.net/en/api/)
	- Cryptopia: not implemented

Extra:
- switch to akka
- make smarter:
	- move across exchanges (when needed/available)
	- cancel orders if they'd no longer expect fill / be profitable
	- move all assets into coin expected to rise most
		- yet spread risk?
- deal with pings without available exchange ticker:
	- use signal as ASK reference to make limit order
	- verify ping by % up
- linalg/ML: tweak params, infer fees/time
