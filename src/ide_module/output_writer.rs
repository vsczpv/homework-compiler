
use std::io::{self, Write};
use std::fs::OpenOptions;




pub struct Writer {
  out_path: String,
}


impl Writer {
  pub fn new() -> Self {
    Writer { out_path: String::from("IDE/outputs/") }
  }
  pub fn write(&self, file_name: &str, text: &str) -> io::Result<()> {
    let mut doc_path = String::from("");
    doc_path.push_str(&self.out_path.clone());
    doc_path.push_str(file_name);

    let mut file = OpenOptions::new()
    .write(true)
    .create(true)
    .truncate(true)
    .open(doc_path)?;

    file.write_all(b"")?;
    write!(file, "{}",text)?;
    Ok(())
  }
}