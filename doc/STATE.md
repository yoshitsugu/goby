# Goby Project State Snapshot

Last updated: 2026-04-06

## Current Focus

Track LM (Mutable List Element Assignment) が完了した。
現時点での明確な次アクティブトラックはない。

候補:

1. **Track TD2 続行** (`doc/PLAN_ERROR.md`) — 型付き診断スパンの残余カバレッジ
   (multiline/body-relative expressions が deferred のまま)
2. **List index precedence 修正** (`PLAN.md §2.1`) — `f xs[0]` が `(f xs)[0]` になる
   現パーサーの誤り修正
3. **Track Float** (`PLAN.md §4.7`) — `Float` 型の追加

## Recently Completed

### Track LM: Mutable List Element Assignment (2026-04-06)

`a[i] := v` および `a[i][j] := v` が Goby コンパイラの全パイプラインで動作。

- LM0: `LANGUAGE_SPEC.md` §3 にセマンティクスを記述
- LM1a–c: `AssignTarget` / `ResolvedTarget` AST 拡張、パーサー、リゾルバー
- LM2: 型チェック (`check_assign_target_chain`、7 件のテスト)
- LM3a: IR `CompExpr::AssignIndex` ノード追加
- LM3b: `ir_lower.rs` で `ResolvedTarget::ListIndex` → `AssignIndex` 変換
- LM3c: Wasm バックエンド `BackendIntrinsic::ListSet` (path-copy alloc) + `lower_assign_index`
- LM4: `examples/mut_list.gb`、4 件のランタイム統合テスト

最終テスト数: 688 goby-core + 560 goby-wasm + 62 integration = 全パス

### Track CC: Closure Capture (2026-04-02)

CC0–CC6 全完了。`GeneralLowered` Wasm パスでクロージャキャプチャが正常動作。
フォールバック/インタープリターランタイムも同じセマンティクスに揃った。

### Track TD2: Typed Diagnostic Spans (2026-04-05 時点)

effect-op および `resume` 引数不一致の診断スパンが `goby-core` に実装済み。
残余: multiline/body-relative expressions の honest span ownership (deferred)。

## Architecture State

| レイヤー | 状態 |
|---|---|
| Parser (`parser_stmt.rs`) | `AssignTarget` でリストインデックス代入を解析可能 |
| Resolver (`resolved.rs`) | `ResolvedTarget` で代入先を表現 |
| Typechecker | `check_assign_target_chain` でリストインデックス代入を検証 |
| IR (`ir.rs`) | `CompExpr::AssignIndex` 実装済み |
| IR lowering (`ir_lower.rs`) | `lower_list_index_assign` で path-copy 変換 |
| Wasm backend | `BackendIntrinsic::ListSet` + `lower_assign_index` 実装済み |
| Effect handlers | 非末尾/multi-resume は `BackendLimitation` のまま |
| Resolved→IR 境界 | 安定。`doc/PLAN_IR.md` がリファレンス |
| GC/reclamation | 対象外。バンプアロケーターのまま |

## Known Deferred Items

- `f xs[0]` のパーサー優先度誤り (`PLAN.md §2.1`)
- multiline block 引数のスパン所有権 (`doc/PLAN_ERROR.md`)
- `Float` 型 (`PLAN.md §4.7`)
- effect runtime を `EffectId`/`OpId` テーブルに移行 (`PLAN.md §5`)
- `doc/BUGS.md` の nested list 評価 (fallback ランタイム)

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — トップレベルロードマップ
- [`doc/PLAN_ERROR.md`](PLAN_ERROR.md) — 型付き診断スパン計画 (次候補)
- [`doc/PLAN_LIST_MUT.md`](PLAN_LIST_MUT.md) — 完了済み LM 詳細計画
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — 現在の言語仕様
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering 境界設計リファレンス
- [`doc/BUGS.md`](BUGS.md) — 既知バグトラッカー
