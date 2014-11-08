//Name: Nicholas Noto
//CruzID: nnoto

#include <cstdio>
#include <iostream>
#include <string>
#include <iomanip>
#include <vector>
using namespace std;

void ksa(vector<unsigned char> &s, string key);
void prga(vector<unsigned char> &s, vector<unsigned char> &out, int keyLength);

int main() {
  vector<unsigned char> state(256);
  vector<unsigned char> stream(1024);
  string plaintext = "", key = "";
  int KeyLength = 16;
  cout << "Enter Key: ";
  getline(cin, key);
  cout << "Enter text: ";
  getline(cin, plaintext);
  cout << "Text to encrypt: " << plaintext << endl;
  ksa(state,key);
  prga(state,stream, KeyLength);
  cout << "Cipher: ";
  for (int i=0; i < plaintext.length(); i++){
    //XOR plaintext with keystream
    plaintext[i] ^= stream[i];
    printf("%02X ", (unsigned char)plaintext[i]);
  }
  cout << "\nText Decrypted: ";
  for (int i=0; i < plaintext.length(); i++){
    plaintext[i] ^= stream[i];
    cout << plaintext[i];
  }
  cout << endl;  
  return 0;
}

// Key Scheduling Algorithm
void ksa(vector<unsigned char> &s, string key) {
  int j = 0, temp = 0;
  //initialize s array
  for(int i = 0; i < 256; i++)
    s[i] = i;
  for(int i = 0; i < 256; i++) {
    j = (j + s[i] + key[i % key.length()]) % 256;
    temp = s[i];
    s[i] = s[j];
    s[j] = temp;
  }
}

// Pseudo-Random Generator Algorithm
void prga(vector<unsigned char> &s,vector<unsigned char> &output, int keyLength) {
  int j = 0, k = 0, temp = 0;
  cout << "Keystream: ";
  for(int i = 0; i < keyLength; i++)  {
    k = (k + 1) % 256;
    j = (j + s[k]) % 256;
    temp = s[k];
    s[k] = s[j];
    s[j] = temp;
    output[i] = s[(s[k] + s[j]) % 256];
    printf("%02X ", output[i]);
  }
  cout << endl;
}
