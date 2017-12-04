all : texture-albedo.gnp map_0.gnp

texture-albedo.gnp : texture.psd
	del texture-*.*
	magick -dispose background texture.psd -layers coalesce texture-layer%d.png
	del texture-layer0.png
	rename texture-layer1.png texture-albedo.gnp
	rename texture-layer2.png texture-ao.gnp
	rename texture-layer3.png texture-emission.gnp
	rename texture-layer4.png texture-height.gnp
	rename texture-layer5.png texture-roughness.gnp
	rename texture-layer6.png texture-metallic.gnp
	rename texture-layer7.png texture-normal.gnp
	
map_0.gnp : map.psd
	del map_*.*
	magick -dispose background map.psd -layers coalesce map_%d.png
	del map_0.png
	rename map_1.png map_1.gnp
	rename map_2.png map_2.gnp
	rename map_3.png map_3.gnp