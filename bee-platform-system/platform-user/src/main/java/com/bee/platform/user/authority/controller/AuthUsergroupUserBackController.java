package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.rq.AuthUsergroupRalationUserRQ;
import com.bee.platform.user.authority.service.AuthUsergroupUserBackService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 用户组和user关联表 前端控制器
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authUsergroupUserBack", tags = "新权限：用户组和用户关联关系相关接口-后台")
@RequestMapping("/authUsergroupUserBack")
public class AuthUsergroupUserBackController {

    @Autowired
    private AuthUsergroupUserBackService authUsergroupUserService;


    @ApiOperation(value = "给用户组分配用户")
    @PostMapping("/usergroupRelationUser")
    public ResponseResult<String> usergroupRelationUser(@RequestBody @Valid AuthUsergroupRalationUserRQ rq) {

        if (ObjectUtils.isEmpty(rq) || ObjectUtils.isEmpty(rq.getUsergroupId())) {
            log.info("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        authUsergroupUserService.usergroupRalationUser(rq.getUsergroupId(), rq.getUserIds());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation(value = "查询用户组下的用户列表")
    @PostMapping("/getUsergroupUserList")
    public ResponseResult<List<AuthUsergroupUserListDTO>> getUsergroupUserList(@RequestParam Integer usergroupId) {

        if (ObjectUtils.isEmpty(usergroupId)) {
            log.info("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        List<AuthUsergroupUserListDTO> dto = authUsergroupUserService.getUsergroupUserList(usergroupId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

