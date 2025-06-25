

/* Botão Compilar (executa o compilador) */
document.getElementById('compile_button').addEventListener('click', async () => {
      const btn = document.getElementById('compile_button')
      btn.disabled = true
      btn.textContent = 'compiling...'
      const code = document.getElementById("user_code")?.value.toString()?? ""
      try {
        const res = await fetch(`/run?user_code=${code}`)
        const data = await res.json()
        document.getElementById('assembly_code').textContent = data.assembly_code
        document.getElementById('error').textContent = data.error
        
      } catch (e) {
        alert('Erro: ' + e.message)
      } finally {
        btn.disabled = false
        btn.textContent = 'Rodar Programa'
      }
    })


/* permite usar tab no código */
const textarea = document.getElementsByTagName('textarea').addEventListener('keydown', function(e) {
    if (e.key === 'Tab') {
      e.preventDefault();
      const start = this.selectionStart;
      const end = this.selectionEnd;

      this.value = this.value.substring(0, start) + "\t" + this.value.substring(end);

      // Volta o foco para a área de código
      this.selectionStart = this.selectionEnd = start + 1;
    }
  });