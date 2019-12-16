package com.bee.platform.user.controller;

import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.NewsService;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @ClassName: 资讯相关接口
 * @Description:
 * @Author: fei.sun
 * @Date: 2019/4/25 14:41
 * @Version: 1.0
 */

@RestController
@RequestMapping("/news")
@Api(value = "资讯API", tags = "资讯相关API")
public class NewsManageController {
    @Autowired
    NewsService newsService;

    @Autowired
    private ManageUserService userService;

    /*@ApiOperation(value = "*****发表资讯")
    @RequestMapping(value = "/manager/news/publish", method = RequestMethod.POST)
    public ResponseResult addOrUpdateNews(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid NewsInfoRQ newsInfoRQ) {
        ManagerInfo managerInfo = userService.getManagerInfo(sysToken);
        ResponseResult result;
        try {
            result = newsService.addOrUpdateNews(managerInfo.getManagerId(), newsInfoRQ);
        } catch (Exception e) {
            e.printStackTrace();
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }*/

}
