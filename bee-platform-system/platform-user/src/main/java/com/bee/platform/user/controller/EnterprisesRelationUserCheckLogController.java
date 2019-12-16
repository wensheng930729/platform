package com.bee.platform.user.controller;


import com.bee.platform.common.entity.*;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.EnterprisesRelationUserCheckLogService;
import com.bee.platform.user.service.UsersService;
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
 * 企业关联用户审核日志表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "企业关联用户审核日志相关接口", tags = "企业关联用户审核日志相关接口")
@RestController
@RequestMapping("/enterprisesRelationUserCheckLog")
public class EnterprisesRelationUserCheckLogController {

    @Autowired
    private AuthPlatformUserService usersService;
    @Autowired
    private EnterprisesRelationUserCheckLogService enterprisesRelationUserCheckLogService;

    @ApiOperation(value = "查询企业关联用户审核日志记录信息", notes = "查询企业关联用户审核日志记录信息")
    @GetMapping("/getCheckLogs")
    public ResponseResult getCheckLogs(HttpServletRequest request, Page page) {
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("用户信息查询失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterprisesRelationUserCheckLogService.getCheckLogs(userInfo, page);
    }


}

