package com.bee.platform.dinas.datadriver.controller;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.service.feign.AuthResourcesFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * @ClassName DinasCommonController
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/8/19$ 10:25$
 * @version 1.0.0
 */
@RestController
@RequestMapping("/dinasCommon")
@Api(value = "DinasCommonController", tags = "砂石公共方法相关接口")
public class DinasCommonController {

    @Autowired
    private AuthResourcesFeignClient resourcesFeignClient;

    @GetMapping("/resourcesByDriver")
    @ApiOperation(value = "获得资源-录入系统")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(HttpServletRequest request,
                                                                           @RequestParam String subSys, @RequestParam(required = false) String sysToken){
        return resourcesFeignClient.listResourcesByUser(subSys,sysToken);
    }
}
