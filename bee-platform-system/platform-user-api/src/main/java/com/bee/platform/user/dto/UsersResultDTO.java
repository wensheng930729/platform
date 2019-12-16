package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户信息返回对象
 * @author: junyang.li
 * @create: 2019-03-14 15:01
 **/
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("用户基础数据返回对象")
public class UsersResultDTO  implements Serializable {

    private static final long serialVersionUID = -6774815462341968473L;
    private Integer id;

    private String name;
}
