---
effort: small
priority: baja
risk: low
scope: Public/delphi-lookup/Workspace.Support.pas (durmiente) + delphi-lookup.dproj
  + uResultFormatter.pas (si se integra)
status: open
type: idea
value: low
---
# 01 — Decidir qué hacer con `Workspace.Support.pas` (durmiente)

**Estado:** archivo presente en el repo, **no referenciado por ningún proyecto**
(`delphi-lookup.dpr`/`.dproj` ni `delphi-indexer.*` lo incluyen) → no se compila, es
código durmiente. Staged como `new file` el 2026-06-19.

## Origen

El 2026-06-19 se recuperó un `git stash` (vía `stash pop` con conflictos). El análisis
mostró que el stash era **~95 % obsoleto**: un snapshot anterior a varios commits de
`main` cuyo lado "Stashed changes" *revertía* fixes ya integrados, entre ellos:

- `UNIQUE(name,file_path,type,start_line)` para preservar overloads (commit `fa492f7`…)
  → el stash volvía a `UNIQUE(name,file_path,type)`.
- columna `is_declaration` + su migración (commit `d00bea8`) → ausente en el stash.
- índices `COLLATE NOCASE` (short-circuit de identificadores) → el stash usaba índices
  `BINARY`.
- salida ultra-compacta + `--json` + flags semánticos + tabla `query_cache` en
  `delphi-lookup.dpr` → versión vieja sin nada de eso.
- `uVectorSearch.ReadMetadata()` añadido por el stash pero **nunca llamado** (código
  muerto, descartado).

**Decisión tomada:** todos los conflictos se resolvieron a favor de `main` (HEAD),
conservando los fixes. Lo único salvable era la feature nueva `Workspace.Support`, que
se dejó **durmiente** a la espera de decidir.

## Qué hace la feature

`Workspace.Support.pas` (≈433 líneas, autocontenido, sin dependencias del código viejo):

- `DetectWorkspace: TWorkspaceInfo` — detecta si el CWD está dentro de un workspace de
  git-worktree de CyberMAX (vía env `CYBERMAX_WORKSPACE`, patrón `/workspaces/<name>/`,
  `.workspace.json` con `worktree_repos`, o fallback escaneando `.git`).
- `TranslatePathForWorkspace(APath, AWorkspace)` — traduce rutas de la BD
  (`W:\Repo\...`) a la ruta del workspace activo
  (`W:\Repos\workspaces\<name>\Repo\...`), solo para repos que son worktree.

La integración (que vivía en el stash y **no** se aplicó) era pequeña: `uses
Workspace.Support` en `uResultFormatter` + campos `FWorkspace/FWorkspaceChecked` +
métodos `GetWorkspace`/`TranslateFilePath` + traducir la ruta en `FormatSingleResult`
añadiendo una línea `// Path:`; más `uses` en `delphi-lookup.dpr` y dar de alta el
`.pas` en `delphi-lookup.dproj`.

## Opciones

1. **Integrar** — injertar la feature sobre `main` actual (conservando todos los
   métodos nuevos de HEAD: ExtractSignature, ultra-compact, JSON…), darla de alta en
   `delphi-lookup.dproj`, y compilar para verificar. Útil si se trabaja con worktrees y
   se quiere que los paths de los resultados apunten al workspace activo.
2. **Retirar** — borrar `Workspace.Support.pas`. Queda en el historial si se necesita.
3. **Dejar durmiente** — no hacer nada (estado actual).

## Cierre

Al resolver: implementar la opción elegida, borrar este `.md` y actualizar el índice
en `TO-DO/CLAUDE.md`.
