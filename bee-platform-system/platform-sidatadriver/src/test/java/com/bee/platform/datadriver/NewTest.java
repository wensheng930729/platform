package com.bee.platform.datadriver;


import com.bee.platform.Application;
import com.bee.platform.common.service.JedisService;

import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @ClassName: NewTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/25 15:45
 * @Version: 1.0
 */

@Slf4j
@SpringBootTest(classes={Application.class})
@RunWith(SpringRunner.class)
public class NewTest {

    @Autowired
	private JedisService jedisService;
	
    @Test
    public void test(){
    	Integer incr = jedisService.incrOne("test-001");
    	System.out.println(incr);

    }
}
