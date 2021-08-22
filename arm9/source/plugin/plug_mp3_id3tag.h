
typedef struct {
  bool Enabled;
  char title[31];
  char artist[31];
  char album[31];
  char year[5];
  char comment[31];
  byte genre;
} TID3Tag;

static TID3Tag ID3Tag;

static const char* GetGenreStr(u8 Genre)
{
  // NOTE: These genre names should be compliant to ID3v1 and the Winamp extended set. Fixed by chuckstudios
  
#define ID3Tag_GenreCount (148)
static const char ID3Tag_Genre[ID3Tag_GenreCount][24]={
"Blues","Classic Rock","Country","Dance","Disco","Funk","Grunge","Hip-Hop","Jazz","Metal",
"New Age","Oldies","Other","Pop","R&B","Rap","Reggae","Rock","Techno","Industrial",
"Alternative","Ska","Death Metal","Pranks","Soundtrack","Euro-Techno","Ambient","Trip-Hop","Vocal","Jazz+Funk",
"Fusion","Trance","Classical","Instrumental","Acid","House","Game","Sound Clip","Gospel","Noise",
"AlternRock","Bass","Soul","Punk","Space","Meditative","Instrumental Pop","Instrumental Rock","Ethnic","Gothic",
"Darkwave","Techno-Industrial","Electronic","Pop-Folk","Eurodance","Dream","Southern Rock","Comedy","Cult","Gangsta",
"Top 40","Christian Rap","Pop/Funk","Jungle","Native American","Cabaret","New Wave","Psychadelic","Rave","Showtunes",
"Trailer","Lo-Fi","Tribal","Acid Punk","Acid Jazz","Polka","Retro","Musical","Rock & Roll","Hard Rock",
"Folk","Folk-Rock","National Folk","Swing","Fast Fusion","Bebob","Latin","Revival","Celtic","Bluegrass",
"Avantgarde","Gothic Rock","Progressive Rock","Psychedelic Rock","Symphonic Rock","Slow Rock","Big Band","Chorus","Easy Listening","Acoustic",
"Humour","Speech","Chanson","Opera","Chamber Music","Sonata","Symphony","Booty Bass","Primus","Porn Groove",
"Satire","Slow Jam","Club","Tango","Samba","Folklore","Ballad","Power Ballad","Rhythmic Soul","Freestyle",
"Duet","Punk Rock","Drum Solo","A capella","Euro-House","Dance Hall","Goa","Drum & Bass","Club-House","Hardcore",
"Terror","Indie","BritPop","Negerpunk","Polsk Punk","Beat","Christian Gansta Rap","Heavy Metal","Black Metal","Crossover",
"Contemporary Christian","Christian Rock","Merengue","Salsa","Thrash Metal","Anime","JPop","Synthpop",
};

  if(ID3Tag_GenreCount<=Genre) return("Genre not found.");
  return(ID3Tag_Genre[Genre]);
}

static void ReadID3TAG_TrimString(char *pstr,u32 strlen)
{
  if(0x80<=pstr[strlen-1]) pstr[strlen-1]=0;
  pstr[strlen]=0;
  u32 pos=(u32)-1;
  for(u32 idx=0;idx<strlen;idx++){
    if(pstr[idx]==0) break;
    if(pstr[idx]!=0x20) pos=idx;
  }
  if(pos==(u32)-1){
    pstr[0]=0;
    }else{
    pstr[pos+1]=0;
  }
}

static void ReadID3TAG(void)
{
  if(FileSize<128){
    ID3Tag.Enabled=false;
    return;
  }
  
  char buf[128];
  
  FAT2_fseek(FileHandle,FileSize-128,SEEK_SET);
  FAT2_fread(buf,1,128,FileHandle);
  FAT2_fseek(FileHandle,0,SEEK_SET);
  
  if((buf[0]!='T')||(buf[1]!='A')||(buf[2]!='G')){
    ID3Tag.Enabled=false;
    return;
  }
  
  ID3Tag.Enabled=true;
  
  char *pstr;
  u32 strlen;
  
  pstr=ID3Tag.title;
  strlen=30;
  MemCopy8CPU(&buf[3],pstr,strlen);
  ReadID3TAG_TrimString(pstr,strlen);
  
  pstr=ID3Tag.artist;
  strlen=30;
  MemCopy8CPU(&buf[33],pstr,strlen);
  ReadID3TAG_TrimString(pstr,strlen);
  
  pstr=ID3Tag.album;
  strlen=30;
  MemCopy8CPU(&buf[63],pstr,strlen);
  ReadID3TAG_TrimString(pstr,strlen);
  
  pstr=ID3Tag.year;
  strlen=4;
  MemCopy8CPU(&buf[93],pstr,strlen);
  ReadID3TAG_TrimString(pstr,strlen);
  
  pstr=ID3Tag.comment;
  strlen=30;
  MemCopy8CPU(&buf[97],pstr,strlen);
  ReadID3TAG_TrimString(pstr,strlen);
  
  ID3Tag.genre=(byte)buf[127];
}

