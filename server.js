import {createServer} from 'node:http';
import { spawn } from 'node:child_process'
import { promises as fs } from 'node:fs'
import { join, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'


const __filename = fileURLToPath(import.meta.url)
const __dirname  = dirname(__filename)


const server = createServer(async (req, res) => {
  const url = new URL(req.url, `http://${req.headers.host}`)

  console.log(`pathname: ${url.pathname}`)

  // 1) Rota raiz: enviar html
  if (url.pathname === '/') {
    try {
      const html = await fs.readFile(join(__dirname, 'IDE', 'index.html'))
      res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' })
      return res.end(html)
    } catch (err) {
      res.writeHead(500)
      return res.end('Erro ao carregar HTML')
    }
  } else

  // 2) Rota /run: executa systemL
  if (url.pathname === '/run') {
    
    /* --- coleta compilador --- */
    const bin = join(__dirname, 'target/debug/systeml.exe')
    let m_exitCode
    let m_stderr = ""

    /* coleta código do usuário */
    let user_code = ''
    req.on('data', chunk => {
      user_code += chunk.toString()
    })


    req.on('end', async () => {
      
      /* --- escreve arquivo com código do usuário --- */
      //console.log(`código: ${user_code}`)
      const code_path = "IDE/outputs/user_code.l"
      await fs.writeFile(code_path, user_code)
      try {

        /* --- Roda compilador --- */
        m_exitCode = await new Promise((resolve, reject) => {
          const proc = spawn(bin, [code_path], { cwd: __dirname })

          proc.stderr.on('data', chunk => {
            m_stderr += (chunk?.toString()?? "")
          })

          proc.on('close', code => resolve(code))
        })
      } catch (err) {
        console.log("erro1")
        res.writeHead(500)
        return res.end(JSON.stringify({ error: 'Falha ano spawn: ' + err + "\n" + m_stderr}))
      }
      
      try{
        /* --- Lê e devolve arquivos gerados --- */
        const assembly_code = await fs.readFile(join(__dirname, 'IDE/outputs/', 'assembly_code.txt'), 'utf8')
        const typed_tree    = await fs.readFile(join(__dirname, 'IDE/outputs/', 'typed_tree.txt'), 'utf8')
        const symble_table  = await fs.readFile(join(__dirname, 'IDE/outputs/', 'symble_table.txt'), 'utf8')

        res.writeHead(200, { 'Content-Type': 'application/json; charset=utf-8' })
        const responseObject = {
          'typed_tree': typed_tree,
          'symble_table': symble_table,
          'assembly_code': assembly_code,
          'error': m_stderr
        }
        return res.end(JSON.stringify(responseObject))
      } catch (err) {
        res.writeHead(500)
        console.log("FALHA AO LER ARQUIVOS")
        return res.end(JSON.stringify({ error: 'Falha ao ler arquivos: ' + err }))
      }
    })


  }

  // 404
  else {
    res.writeHead(404)
    return res.end('Not Found')
  }
})

server.listen(3333, () => {
    console.log("porta");
})
.on('error', function (err) {
    console.log("erro na leitura da porta");
    console.log(err);
});
