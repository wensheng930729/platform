package com.bee.platform.user;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.rq.AuthInterfaceRQ;
import com.bee.platform.user.authority.service.AuthInterfaceService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class AuthInterfaceServiceTest {

    @Autowired
    private AuthInterfaceService authInterfaceService;
    
    /**
     * 测试根据路由、URL、请求方式添加接口
     */
    @Test
    public void testSaveOrUpdateByRouterUrlMethod() {
    	AuthInterfaceRQ authInterfaceRQs = new AuthInterfaceRQ()
    			.setBeeRouter("xxx")
    			.setName("xxxBeat")
    			.setType("POST")
    			.setSubSys("XXX")
    			.setOrderNum(1)
    			.setUrl("/beetrade-business/xxx");
		ResponseResult<ResCodeEnum> responseResult = authInterfaceService.addByRouterUrlMethod(authInterfaceRQs);
		log.info("code={}, msg={}", responseResult.getCode(), responseResult.getMessage());
    }
	
}
