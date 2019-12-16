package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AuthUserRoleRQ
 * @Description 功能描述
 * @Date 2019/5/29 9:57
 **/


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("添加修改删除用户关联角色功能应用请求参数")
public class AuthUserRoleRQ implements Serializable {

    private static final long serialVersionUID = 9201784476170148394L;

    @ApiModelProperty("企业用户关联角色功能应用请求参数")
    private  List<UserRelationRoleRQ> addList;

    @ApiModelProperty("企业用户关联角色功能应用请求参数")
    private  List<UserRelationRoleRQ> deleteList;
}
