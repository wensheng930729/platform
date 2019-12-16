package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-21 11:11
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class NewDetailDTO  implements Serializable {

    private static final long serialVersionUID = 6389485670387925668L;
    private Integer id;

    private String title;

    private String image;

    private String newsSource;

    private Date createAt;

    private Date updateAt;

    private Integer userId;

    private Integer hits;

    private Integer type;

    private Integer state;

    private String content;
}
