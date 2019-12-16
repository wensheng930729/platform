package com.bee.platform.datadriver;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import com.bee.platform.Application;
import com.bee.platform.common.dto.AuthInterfaceRQ;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@SpringBootTest(classes={Application.class})
@RunWith(SpringRunner.class)
public class UserInfoUtilsTest {

	@Qualifier(value="authUserInfoUtils")
	@Autowired
	private UserInfoUtils userInfoUtils;
	
	@Test
	public void testUserInfoUtilsGetSystemConf() {
		String conf = userInfoUtils.getSystemConf("default_password", null, null);
		log.info("conf={}", conf);
	}
	
	@Test
	public void testUserInfoUtilsAddByRouterUrlMethod() {
		AuthInterfaceRQ authInterfaceRQ = new AuthInterfaceRQ()
				.setBeeRouter("xxx")
				.setUrl("/beetrade-business/xxx")
				.setType("POST")
				.setName("xxxBeat")
				.setSubSys("XXX")
				.setOrderNum(1);
		ResponseResult<ResCodeEnum> responseResult = userInfoUtils.addByRouterUrlMethod(authInterfaceRQ );
		log.info("code={}, msg={}", responseResult.getCode(),responseResult.getMessage());
	}
	
}
