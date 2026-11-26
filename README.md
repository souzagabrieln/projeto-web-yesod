> *Note: This project was generated from the `yesod-minimal` scaffolding, and does not support features like `yesod devel`. If you want these features, use the `yesod-simple` stack template.*

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install GHC: `stack setup`
3. Build libraries: `stack build`

## Development

Start a development server with:

```
stack build --exec test-minimal
```

## Banco de dados (PostgreSQL)

A conexão com o banco é montada em `app/Main.hs` a partir de variáveis de ambiente.
Se uma variável não estiver definida, o valor padrão da tabela é usado.

| Variável      | Padrão      | O que é              |
|---------------|-------------|----------------------|
| `DB_HOST`     | `localhost` | Endereço do servidor |
| `DB_PORT`     | `5432`      | Porta do servidor    |
| `DB_USER`     | `fulano`    | Usuário do banco     |
| `DB_PASSWORD` | `123`       | Senha do usuário     |
| `DB_NAME`     | `proj`      | Nome do banco        |

No Windows PowerShell, defina as variáveis antes de rodar o projeto, no mesmo terminal:

```powershell
$env:DB_USER = "postgres"
$env:DB_PASSWORD = "sua_senha"
$env:DB_NAME = "proj"
stack run
```

Essas variáveis valem só para aquele terminal. Ao abrir um novo, defina de novo.
O banco indicado em `DB_NAME` precisa existir (crie com `CREATE DATABASE proj;` no psql ou pgAdmin).
Nunca coloque a sua senha real no código nem faça commit dela.

No Linux ou macOS:

```bash
DB_USER=postgres DB_PASSWORD=sua_senha DB_NAME=proj stack run
```

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
# yesod-minimal-psql-static
