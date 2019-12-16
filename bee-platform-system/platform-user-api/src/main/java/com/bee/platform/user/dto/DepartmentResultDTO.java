package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 部门
 * @author: junyang.li
 * @create: 2019-03-14 15:07
 **/
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("用户部门基础数据返回对象")
public class DepartmentResultDTO implements Serializable {

    private static final long serialVersionUID = 986744787004861352L;
    private Integer id;

    private String name;
}
