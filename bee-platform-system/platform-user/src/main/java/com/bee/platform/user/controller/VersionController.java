package com.bee.platform.user.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.user.dto.VersionDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;


@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/version")
@Api(value = "版本控制类", tags = "版本控制类")
public class VersionController {

    @Autowired
    private ConfigService configService;

    @ApiOperation(value="获取版本号")
    @RequestMapping(value = "/getVersion",method = RequestMethod.GET)
    public ResponseResult<VersionDTO> getVersion(HttpServletRequest request){
        String version  = configService.getConfigByconfigKey("platform_version").getConfigValue();
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new VersionDTO().setVersion(version));
    }

}

