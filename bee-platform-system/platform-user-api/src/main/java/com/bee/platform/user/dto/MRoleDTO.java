package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @description:  云平台后台角色列表
 * @author: junyang.li
 * @create: 2019-04-30 09:34
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "云平台后台角色列表")
public class MRoleDTO implements Serializable {

    private static final long serialVersionUID = 1889787519630384032L;
    /**
     * 角色id
     */
    @ApiModelProperty("组id")
    private Integer roleId;
    /**
     * 角色名称
     */
    @ApiModelProperty("组id")
    private String roleName;
    /**
     * 角色类型
     */
    @ApiModelProperty("组id")
    private Integer roleType;
    /**
     * 角色状态
     */
    @ApiModelProperty("组id")
    private Integer status;

    @ApiModelProperty("是否被选中")
    private Boolean checked;
    /**
     * 子角色
     */
    @ApiModelProperty("组id")
    private List<MRoleDTO> child;

    /**
     * @notes 返回子菜单的list对象
     * @Author junyang.li
     * @Date 15:00 2019/1/14
     **/
    public List<MRoleDTO> isNull(){
        if(child==null){
            this.child=new ArrayList<>();
        }
        return this.child;
    }
}
