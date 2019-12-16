package com.bee.platform.user;


import com.bee.platform.user.dto.NewsDTO;
import com.bee.platform.user.entity.News;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeanUtils;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @ClassName: NewsTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/25 15:45
 * @Version: 1.0
 */

@Slf4j
@SpringBootTest(classes={Application.class})
@RunWith(SpringRunner.class)
public class NewsTest {

    @Test
    public void test(){
//        NewsDTO newDTO = new NewsDTO();
//        newDTO.setType(0);
//        newDTO.setTitle("呵呵");
//        newDTO.setStartTime("2019-04-25");
//        newDTO.setStartTime("2019-04-26");
//        News news = new News();
//        BeanUtils.copyProperties(newDTO, news);
//        System.out.println("news : "+news);
        
        for (int i = 0; i < 10; i++) {
			 for (int j = 0; j < 10; j++) {
				 for (int j2 = 0; j2 < 10; j2++) {
					return;
				}
			}
		}
        System.out.println("---");
    }
    
    public static void main(String[] args) {
    	 for (int i = 0; i < 10; i++) {
			 for (int j = 0; j < 10; j++) {
				 for (int j2 = 0; j2 < 10; j2++) {
					return;
				}
			}
		}
        System.out.println("---");
	}
}
