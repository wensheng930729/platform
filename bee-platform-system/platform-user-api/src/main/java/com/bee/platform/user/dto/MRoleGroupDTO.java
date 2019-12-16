package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * @description: 后台权限列表
 * @author: junyang.li
 * @create: 2019-05-08 14:42
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "后台权限列表")
public class MRoleGroupDTO {


    @ApiModelProperty("组id")
    private Integer groupId;


    @ApiModelProperty("组名称")
    private String groupName;

    @ApiModelProperty("权限")
    private List<MRoleDTO> roles;

    /**
     * @notes 返回子菜单的list对象
     * @Author junyang.li
     * @Date 15:00 2019/1/14
     **/
    public List<MRoleDTO> isNull(){
        if(roles==null){
            this.roles=new ArrayList<>();
        }
        return this.roles;
    }
}
