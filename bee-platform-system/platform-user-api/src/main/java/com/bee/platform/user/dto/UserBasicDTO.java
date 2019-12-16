package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 搜索信息
 * @author: junyang.li
 * @create: 2019-03-14 14:58
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
public class UserBasicDTO implements Serializable {

    private static final long serialVersionUID = -2259858764139549249L;

    private List<UsersResultDTO> users;

    private List<DepartmentResultDTO> departments;
}
