library(rmarkdown)

get_last_word <- function(sentence) {
  words <- unlist(strsplit(sentence, "\\s+"))
  return(tail(words, 1))
}

get_first_word <- function(sentence) {
  words <- unlist(strsplit(sentence, "\\s+"))
  return(head(words, 1))
}

generate_letter_pdf <- function(
                                isodate,
                                your_name,
                                your_address_line1,
                                your_address_line2,
                                your_contact,
                                your_email,
                                date,
                                recipient_name,
                                recipient_title,
                                office_address_line1,
                                office_address_line2,
                                body) {
  folder_path <- paste(isodate, "letters", sep="_")
  
  # Check if the folder exists, and if not, create it
  if (!file.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created at:", folder_path)
  } else {
    cat("Folder already exists at:", folder_path)
  }
  
  letter_content <- paste0(
    "---\n",
    "title: \"\"\n",
    "output:\n",
    "  pdf_document: default\n",
    "  word_document: default\n",
    "header-includes:\n",
    "  - \\usepackage{graphicx}\n",  # Enables image insertion
    "  - \\usepackage{adjustbox}\n", # Allows size and alignment adjustments
    "  - \\usepackage{parskip}\n",    # Adds spacing between paragraphs
    "  - \\setlength{\\parskip}{1em}\n", # Sets paragraph spacing to 1em
    "---\n\n",
    
    "**", your_name, "**  \n",
    your_address_line1, "  \n",
    your_address_line2, "  \n",
    your_contact, "  \n",
    your_email, "  \n\n",
    
    date, "   \n\n",
    
    "\\vspace{1.5em}\n\n",
    
    "**", recipient_name, "**  \n",
    recipient_title, "  \n",
    office_address_line1, "  \n",
    office_address_line2, "  \n\n",
    
    paste("Mahal na",
          get_first_word(recipient_name),
          paste0(get_last_word(recipient_name),",\n\n")),
    
    paste(body, collapse = "\n\n"), "\n\n",
  
    "\\vspace{1.5em}\n\n",
    
    "Taos-pusong gumagalang,\n\n",
    "\\vspace{-1em}\n\n",
    "\\noindent\\includegraphics[width=1.5in]{", "signature.png", "}\n\n",
    
    "\\vspace{-2em}\n\n",
    
    your_name
  )
  
  
  writeLines(letter_content, "letter.Rmd")
  
  render("letter.Rmd", output_format = "pdf_document", output_file = paste(
    paste0(folder_path,"/",isodate), get_first_word(recipient_name), get_last_word(recipient_name), sep = "_")
  )
}