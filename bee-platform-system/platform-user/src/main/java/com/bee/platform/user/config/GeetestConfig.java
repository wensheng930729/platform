package com.bee.platform.user.config;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @notes GeetestWeb配置文件
 * @Author junyang.li
 * @Date 15:24 2019/4/28
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@Component
@ConfigurationProperties(prefix = "geetest")
public class GeetestConfig {

	/**
	 * 填入自己的captcha_id
	 */
	private String geetestId ;

	/**
	 * 极验的private_key
	 */
	private String geetestKey ;

	private Boolean newfailBack;


}
