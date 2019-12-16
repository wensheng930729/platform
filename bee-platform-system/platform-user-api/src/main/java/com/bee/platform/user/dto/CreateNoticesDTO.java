package com.bee.platform.user.dto;

import com.bee.platform.common.enums.NoticeTemplateType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 生成通知时需要的参数
 * @author: junyang.li
 * @create: 2019-05-09 10:40
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class CreateNoticesDTO implements Serializable {

    private static final long serialVersionUID = 586495868188559577L;
    /**
     * 通知人id
     */
    private Integer [] notifierIds;

    /**
     * 通知人手机号，短信通知使用
     */
    private String phone;

    /**
     * 通知类型
     */
    private NoticeTemplateType templateType;

    /**
     * 需要的参数
     */
    private Object[] objects;
}
