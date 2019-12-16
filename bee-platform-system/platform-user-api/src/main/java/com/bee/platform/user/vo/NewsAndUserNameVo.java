package com.bee.platform.user.vo;

import com.bee.platform.user.dto.NewDetailDTO;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
public class NewsAndUserNameVo {

	private NewDetailDTO news;
	
	private String username;
}
