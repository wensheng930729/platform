package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpOperationLogDTO;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 操作日志表 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpOperationLog", tags = "erp操作日志相关接口")
@RequestMapping("/erpOperationLog")
public class ErpOperationLogController {

    @Autowired
    private ErpOperationLogService operationLogService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @GetMapping("/get")
    @ApiOperation(value = "根据条件查询erp日志")
    ResponseResult<List<ErpOperationLogDTO>> getOperationLog(HttpServletRequest request, String businessType, Page page, Integer orderId){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
//        if (orderId == null){
//            log.info("参数错误");
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
//        }
        return operationLogService.getOperationLog(businessType,page,userInfo.getOrgId(),orderId);
    }

}

