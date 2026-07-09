library(openssl)

key <- rand_bytes(n = 32)
iv <- rand_bytes(16)

message <- charToRaw("texte à chiffrer")
message
## 74 65 78 74 65 20 c3 a0 20 63 68 69 66 72 65 72

# Chiffrement
ciphertext <- aes_cbc_encrypt(data = message, key = key, iv = iv)
ciphertext
# [1] c1 a5 d6 3f 3e a1 c8 90 39 a5 3d 3d 51 cc c8 4a 97 19 eb f2 f8 17 38 41 4c 65 14 5d ab f7 e3 13
# attr(,"iv")
# [1] d2 84 f3 69 fa d0 8f 28 b8 d4 05 d3 11 12 f5 4b

# Déchiffrement

plaintext <- aes_cbc_decrypt(data = ciphertext, key = key, iv = iv)
plaintext
# [1] 74 65 78 74 65 20 c3 a0 20 63 68 69 66 72 65 72

rawToChar(plaintext)


## test ##

## my side :
message <- charToRaw("super code secret")
ciphertext <- aes_cbc_encrypt(data = message, key = key, iv = iv)
ciphertext
dput(ciphertext)
## à envoyer

## other side : 
## à executer pour le destinataire

ciphertext <- structure(
  as.raw(
    c(0xe1, 0xf2, 0x9e, 0x8e, 0xcf, 0xe6, 0xd2, 
       0xd1, 0xf8, 0x26, 0x13, 0x6a, 0x6e, 0xc4, 0x64, 0xb9, 0xff, 0x01, 
       0x11, 0xa1, 0xf4, 0x3e, 0xe2, 0x67, 0xa6, 0xae, 0x2f, 0xb7, 0x64, 
       0x33, 0xcb, 0xcd
    )
  ),
  iv = as.raw(
    c(
      0xf2, 0x07, 0x39, 0x4b, 0x5f, 
      0x5c, 0x3d, 0x69, 0xcf, 0xe0, 0x0b, 0x44, 0xc4, 0xc1, 0xaa, 0x99
    )
  )
)
ciphertext
plaintext <- aes_cbc_decrypt(data = ciphertext, key = key, iv = iv)
rawToChar(plaintext)

