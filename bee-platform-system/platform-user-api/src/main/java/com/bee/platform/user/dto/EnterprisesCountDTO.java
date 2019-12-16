package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 企业相关的统计，统计企业下部门数量，统计企业中成员数量
 * @author: junyang.li
 * @create: 2019-03-18 11:48
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class EnterprisesCountDTO implements Serializable {

    private static final long serialVersionUID = -8209726843549315578L;

    private Integer orgId;

    private Integer count;
}
