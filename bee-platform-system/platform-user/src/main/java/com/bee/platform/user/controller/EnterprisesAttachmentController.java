package com.bee.platform.user.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import com.bee.platform.user.service.EnterprisesAttachmentService;
import com.bee.platform.user.utils.ValidList;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 企业附件信息表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-25
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "enterprisesAttachment", tags = "企业附件相关接口")
@RestController
@RequestMapping("/enterprisesAttachment")
public class EnterprisesAttachmentController {

    @Autowired
    private AuthPlatformUserService usersService;

    @Autowired
    private EnterprisesAttachmentService enterprisesAttachmentService;

    @ApiOperation(value = "保存附件信息",notes = "保存附件信息")
    @ApiIgnore
    @PostMapping("/saveEnterprisesAttachment")
    public ResponseResult saveEnterprisesAttachment(HttpServletRequest request,  @RequestBody() @Valid ValidList<EnterprisesAttachmentRQ> rqs){
        if(ObjectUtils.isEmpty(rqs)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesAttachmentService.saveEnterprisesAttachment(userInfo,rqs));
    }

}

