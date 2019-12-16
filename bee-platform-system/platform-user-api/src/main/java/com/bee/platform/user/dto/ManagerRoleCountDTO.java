package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @description: 统计角色组下成员数量的实体
 * @author: junyang.li
 * @create: 2019-04-29 16:42
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "统计角色组下成员数量的实体")
public class ManagerRoleCountDTO {

    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 成员数量
     */
    private Integer count;
}
