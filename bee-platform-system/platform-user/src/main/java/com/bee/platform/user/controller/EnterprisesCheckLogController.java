package com.bee.platform.user.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.EnterprisesCheckLogService;
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

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 企业审核日志表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-30
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "企业审核日志相关接口", tags = "企业审核日志相关接口")
@RestController
@RequestMapping("/enterprisesCheckLog")
public class EnterprisesCheckLogController {

    @Autowired
    private EnterprisesCheckLogService enterprisesCheckLogService;
    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "查询企业申请审核日志记录信息", notes = "查询企业申请审核日志记录信息")
    @GetMapping("/getCheckLogs")
    public ResponseResult getCheckLogs(HttpServletRequest request, Page page) {
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        return enterprisesCheckLogService.getCheckLogs(userInfo, page);
    }


}

