package com.bee.platform.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.entity.EnterprisesAppsLog;
import com.bee.platform.user.service.EnterprisesAppsLogService;
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
import java.util.List;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-04-29
 */
@Slf4j
@Api(value = "enterprisesAppsLog", tags = "企业产品审核记录相关接口")
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/enterprisesAppsLog")
public class EnterprisesAppsLogController {

    @Autowired
    private EnterprisesAppsLogService enterprisesAppsLogService;
    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "获取产品审核记录-中台")
    @GetMapping(value = "/listEnterprisesAppsLog")
    public ResponseResult<List<EnterprisesAppsLog>> listEnterprisesAppsLog(HttpServletRequest request, Page page) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination= PageUtils.transFromPage(page);
        List<EnterprisesAppsLog> list = enterprisesAppsLogService.listEnterprisesAppsLog(userInfo,pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

}

