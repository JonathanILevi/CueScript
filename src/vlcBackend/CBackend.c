#include <stdio.h>
#include <vlc/vlc.h>
#include <stdlib.h>
#include <string.h>

struct MediaNode {
	libvlc_media_t* media;
	libvlc_media_player_t* player;
	struct MediaNode* next;
};



libvlc_instance_t* vlcInstance;
struct MediaNode* mediaFiles;



libvlc_media_player_t* getMedia(char* name) {
	libvlc_media_t* media;
	libvlc_media_player_t* player;
	
	media = libvlc_media_new_path(vlcInstance, name);
	player = libvlc_media_player_new_from_media(media);
	
	struct MediaNode* newNode = (struct MediaNode*)malloc(sizeof(struct MediaNode));
	newNode->media = media;
	newNode->player = player;
	newNode->next = mediaFiles;
	mediaFiles = newNode;
	
	return player;
}




void play(char* name) {
	libvlc_media_player_t* thePlayer = getMedia(name);
	libvlc_media_player_play(thePlayer);
}




void setup() {
	vlcInstance = libvlc_new(0, NULL);
	
	if(vlcInstance == NULL) {
		printf("There was an error initializing VLC\n");
		exit(1);
	} else {
		printf("VLC initialized successfully\n");
	}
	mediaFiles = NULL;
}
void cleanUp() {
	while (mediaFiles) {
		printf("free\n");
		struct MediaNode* current = mediaFiles;
		current = mediaFiles;
		mediaFiles = mediaFiles->next;
		
		libvlc_media_release(current->media);
		libvlc_media_player_release(current->player);
		free(current);
	}
	libvlc_release(vlcInstance);
}

