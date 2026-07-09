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

ciphertext <- structure(as.raw(c(0xd5, 0x3d, 0x29, 0xb2, 0xb8, 0x61, 0x7e, 
0xc2, 0x82, 0xf4, 0xdc, 0x4a, 0x7d, 0x78, 0x56, 0x3c, 0x4d, 0xe2, 
0x8a, 0x68, 0xc4, 0x6e, 0x19, 0x5d, 0xb6, 0xb9, 0xd6, 0x50, 0x57, 
0xd7, 0x5c, 0x2d)), iv = as.raw(c(0xd2, 0x84, 0xf3, 0x69, 0xfa, 
0xd0, 0x8f, 0x28, 0xb8, 0xd4, 0x05, 0xd3, 0x11, 0x12, 0xf5, 0x4b
)))
plaintext <- aes_cbc_decrypt(data = ciphertext, key = key, iv = iv)
rawToChar(plaintext)
