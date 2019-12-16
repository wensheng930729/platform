package com.bee.platform.user.service;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.RoleType;
import com.bee.platform.user.entity.UsersDepartments;
import com.baomidou.mybatisplus.service.IService;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface UsersDepartmentsService extends IService<UsersDepartments> {

    /*@Autowired
    private EnterpriseService enterpriseService;

    @ApiOperation(value = "添加部门", notes = "添加部门,企业管理员有此权限")
    @RequestMapping(method = RequestMethod.POST, consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public Object addDepartment(HttpServletRequest request, @RequestParam int department_id,@RequestParam String name) {
        UserInfo userInfo = com.bee.supply.chain.finance.common.utils.UserInfoUtils.getUserInfo(request);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(RoleType.SUPER.getDesc().equals(userInfo.getRoleName())||
                RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())){
            int org_id = userInfo.getOrgId();
            message = enterpriseService.addDepartment(org_id, department_id, name);
            return message;
        }
        message.setCode(-1);
        message.setMessage("没有权限");
        return message;

    }*/
}
